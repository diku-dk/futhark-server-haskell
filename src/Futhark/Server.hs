{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Haskell code for interacting with a Futhark server program.  This
-- module presents a low-level interface.  See
-- <https://futhark.readthedocs.io/en/latest/server-protocol.html the
-- documentation of the server protocol> for the meaning of the
-- commands.  See also "Futhark.Server.Values" for higher-level
-- functions for loading data into a server.
--
-- Error messages produced by the server will be returned as a
-- 'CmdFailure'.  However, certain errors (such as if the server
-- process terminates unexpectedly, or temporary files cannot be
-- created) will result in an IO exception.
--
-- Many of the functions here are documented only as the server
-- protocol command they correspond to.  See the protocol
-- documentation for details.
module Futhark.Server
  ( -- * Server creation
    Server,
    ServerCfg (..),
    newServerCfg,
    withServer,

    -- * Commands
    Cmd,
    CmdFailure (..),
    VarName,
    TypeName,
    EntryName,
    InputType (..),
    OutputType (..),

    -- ** Main commands
    cmdRestore,
    cmdStore,
    cmdCall,
    cmdFree,
    cmdRename,
    cmdInputs,
    cmdOutputs,
    cmdClear,

    -- ** Interrogation
    cmdTypes,
    cmdEntryPoints,

    -- ** Records
    cmdNew,
    cmdProject,
    cmdFields,

    -- ** Auxiliary
    cmdReport,
    cmdPauseProfiling,
    cmdUnpauseProfiling,
    cmdSetTuningParam,
    cmdTuningParams,
    cmdTuningParamClass,

    -- * Utility
    cmdMaybe,
    cmdEither,

    -- * Raw
    startServer,
    stopServer,
    sendCommand,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (removeFile)
import System.Exit
import System.IO hiding (stdin, stdout)
import System.IO.Temp (getCanonicalTemporaryDirectory)
import qualified System.Process as P

-- | The name of a command.
type Cmd = Text

-- | A handle to a running server.
data Server = Server
  { serverStdin :: Handle,
    serverStdout :: Handle,
    serverErrLog :: FilePath,
    serverProc :: P.ProcessHandle,
    serverOnLine :: Cmd -> Text -> IO (),
    serverDebug :: Bool
  }

-- | Configuration of the server.  Use 'newServerCfg' to conveniently
-- create a sensible default configuration.
data ServerCfg = ServerCfg
  { -- | Path to the server executable.
    cfgProg :: FilePath,
    -- | Command line options to pass to the
    -- server executable.
    cfgProgOpts :: [String],
    -- | If true, print a running log of server communication to stderr.
    cfgDebug :: Bool,
    -- | A function that is invoked on every line of input sent by the
    -- server, except the @%%% OK@ and @%%% FAILURE@ prompts.  This
    -- can be used to e.g. print or gather logging messages as they
    -- arrive, instead of waiting for the command to finish.  The name
    -- of the command leading to the message is also provided.  The
    -- default function does nothing.
    cfgOnLine :: Cmd -> Text -> IO ()
  }

-- | Create a server config with the given 'cfgProg' and 'cfgProgOpts'.
newServerCfg :: FilePath -> [String] -> ServerCfg
newServerCfg prog opts =
  ServerCfg
    { cfgProg = prog,
      cfgProgOpts = opts,
      cfgDebug = False,
      cfgOnLine = \_ _ -> pure ()
    }

-- | Start up a server.  Make sure that 'stopServer' is eventually
-- called on the server.  If this does not happen, then temporary
-- files may be left on the file system.  You almost certainly wish to
-- use 'bracket' or similar to avoid this.  Calls 'error' if startup
-- fails.
startServer :: ServerCfg -> IO Server
startServer (ServerCfg prog options debug on_line_f) = do
  tmpdir <- getCanonicalTemporaryDirectory
  (err_log_f, err_log_h) <- openTempFile tmpdir "futhark-server-stderr.log"
  (Just stdin, Just stdout, Nothing, phandle) <-
    P.createProcess
      ( (P.proc prog options)
          { P.std_err = P.UseHandle err_log_h,
            P.std_in = P.CreatePipe,
            P.std_out = P.CreatePipe
          }
      )

  code <- P.getProcessExitCode phandle
  case code of
    Just (ExitFailure e) ->
      error $ "Cannot start " ++ prog ++ ": error " ++ show e
    Just ExitSuccess ->
      error $ "Cannot start " ++ prog ++ ": terminated immediately, but reported success."
    Nothing -> do
      let server =
            Server
              { serverStdin = stdin,
                serverStdout = stdout,
                serverProc = phandle,
                serverDebug = debug,
                serverErrLog = err_log_f,
                serverOnLine = on_line_f
              }
      void (responseLines "startup" server) `catch` onStartupError server
      pure server
  where
    onStartupError :: Server -> IOError -> IO a
    onStartupError s _ = do
      code <- P.waitForProcess $ serverProc s
      stderr_s <- readFile $ serverErrLog s
      removeFile $ serverErrLog s
      error $
        "Command failed with "
          ++ show code
          ++ ":\n"
          ++ unwords (prog : options)
          ++ "\nStderr:\n"
          ++ stderr_s

-- | Shut down a server.  It may not be used again.  Calls 'error' if
-- the server process terminates with a failing exit code
-- (i.e. anything but 'ExitSuccess').
stopServer :: Server -> IO ()
stopServer s = flip finally (removeFile (serverErrLog s)) $ do
  hClose $ serverStdin s
  code <- P.waitForProcess $ serverProc s
  case code of
    ExitSuccess -> pure ()
    ExitFailure _ -> do
      stderr_s <- readFile $ serverErrLog s
      error stderr_s

-- | Start a server, execute an action, then shut down the server.
-- The 'Server' may not be returned from the action.
withServer :: ServerCfg -> (Server -> IO a) -> IO a
withServer cfg m = mask $ \restore -> do
  server <- startServer cfg
  x <- restore (m server) `catch` mException server
  stopServer server
  pure x
  where
    mException server e = do
      -- Anything that goes wrong here is probably less interesting
      -- than the original exception.
      stopServer server `catch` stopServerException e
      throw e
    stopServerException :: SomeException -> SomeException -> IO a
    stopServerException e _ = throw e

-- Read lines of response until the next %%% OK (which is what
-- indicates that the server is ready for new instructions).
responseLines :: Cmd -> Server -> IO [Text]
responseLines cmd s = do
  l <- T.hGetLine $ serverStdout s
  when (serverDebug s) $
    T.hPutStrLn stderr $
      "<<< " <> l
  case l of
    "%%% OK" -> pure []
    _ -> do
      serverOnLine s cmd l
      (l :) <$> responseLines cmd s

-- | The command failed, and this is why.  The first 'Text' is any
-- output before the failure indicator, and the second Text is the
-- output after the indicator.
data CmdFailure = CmdFailure {failureLog :: [Text], failureMsg :: [Text]}
  deriving (Eq, Ord, Show)

-- Figure out whether the response is a failure, and if so, return the
-- failure message.
checkForFailure :: [Text] -> Either CmdFailure [Text]
checkForFailure [] = Right []
checkForFailure ("%%% FAILURE" : ls) = Left $ CmdFailure mempty ls
checkForFailure (l : ls) =
  case checkForFailure ls of
    Left (CmdFailure xs ys) -> Left $ CmdFailure (l : xs) ys
    Right ls' -> Right $ l : ls'

-- Words with spaces in them must be quoted.
quoteWord :: Text -> Text
quoteWord t
  | Just _ <- T.find (== ' ') t =
      "\"" <> t <> "\""
  | otherwise = t

-- | Send an arbitrary command to the server.  This is only useful
-- when the server protocol has been extended without this module
-- having been similarly extended.  Be careful not to send invalid
-- commands.
sendCommand :: Server -> Cmd -> [Text] -> IO (Either CmdFailure [Text])
sendCommand s cmd args = do
  let cmd_and_args' = T.unwords $ map quoteWord $ cmd : args

  when (serverDebug s) $
    T.hPutStrLn stderr $
      ">>> " <> cmd_and_args'

  T.hPutStrLn (serverStdin s) cmd_and_args'
  hFlush $ serverStdin s
  checkForFailure <$> responseLines cmd s `catch` onError
  where
    onError :: IOError -> IO a
    onError e = do
      code <- P.getProcessExitCode $ serverProc s
      let code_msg =
            case code of
              Just (ExitFailure x) ->
                "\nServer process exited unexpectedly with exit code: " ++ show x
              Just ExitSuccess -> mempty
              Nothing -> mempty
      stderr_s <- readFile $ serverErrLog s
      error $
        "After sending command "
          ++ show cmd
          ++ " to server process:"
          ++ show e
          ++ code_msg
          ++ "\nServer stderr:\n"
          ++ stderr_s

-- | The name of a server-side variable.
type VarName = Text

-- | The name of a server-side type.
type TypeName = Text

-- | The name of an entry point.
type EntryName = Text

-- | The type of an input of an entry point.  If 'inputConsumed', then
-- the value passed in a 'cmdCall' must not be used again (nor any of
-- its aliases).
data InputType = InputType
  { inputConsumed :: Bool,
    inputType :: TypeName
  }

-- | The type of an output of an entry point.  If 'outputUnique', then
-- the value returned does not alias any of the inputs.  See the
-- Futhark language manual itself for more details - the implications
-- are quite subtle (but you can ignore them unless you manually use
-- type annotations to make some entry point parameters unique).
data OutputType = OutputType
  { outputUnique :: Bool,
    outputType :: TypeName
  }

inOutType :: (Bool -> TypeName -> a) -> Text -> a
inOutType f t =
  case T.uncons t of
    Just ('*', t') -> f True t'
    Just _ -> f False t
    Nothing -> f False t

helpCmd :: Server -> Cmd -> [Text] -> IO (Maybe CmdFailure)
helpCmd s cmd args =
  either Just (const Nothing) <$> sendCommand s cmd args

-- | @restore filename var0 type0 var1 type1...@.
cmdRestore :: Server -> FilePath -> [(VarName, TypeName)] -> IO (Maybe CmdFailure)
cmdRestore s fname vars = helpCmd s "restore" $ T.pack fname : concatMap f vars
  where
    f (v, t) = [v, t]

-- | @store filename vars...@.
cmdStore :: Server -> FilePath -> [VarName] -> IO (Maybe CmdFailure)
cmdStore s fname vars = helpCmd s "store" $ T.pack fname : vars

-- | @call entrypoint outs... ins...@.
cmdCall :: Server -> EntryName -> [VarName] -> [VarName] -> IO (Either CmdFailure [Text])
cmdCall s entry outs ins =
  sendCommand s "call" $ entry : outs ++ ins

-- | @free vars...@.
cmdFree :: Server -> [VarName] -> IO (Maybe CmdFailure)
cmdFree s = helpCmd s "free"

-- | @rename oldname newname@.
cmdRename :: Server -> VarName -> VarName -> IO (Maybe CmdFailure)
cmdRename s oldname newname = helpCmd s "rename" [oldname, newname]

-- | @inputs entryname@, with uniqueness represented as True.
cmdInputs :: Server -> EntryName -> IO (Either CmdFailure [InputType])
cmdInputs s entry =
  fmap (map (inOutType InputType)) <$> sendCommand s "inputs" [entry]

-- | @outputs entryname@, with uniqueness represented as True.
cmdOutputs :: Server -> EntryName -> IO (Either CmdFailure [OutputType])
cmdOutputs s entry =
  fmap (map (inOutType OutputType)) <$> sendCommand s "outputs" [entry]

-- | @clear@
cmdClear :: Server -> IO (Maybe CmdFailure)
cmdClear s = helpCmd s "clear" []

-- | @report@
cmdReport :: Server -> IO (Either CmdFailure [Text])
cmdReport s = sendCommand s "report" []

-- | @pause_profiling@
cmdPauseProfiling :: Server -> IO (Maybe CmdFailure)
cmdPauseProfiling s = helpCmd s "pause_profiling" []

-- | @unpause_profiling@
cmdUnpauseProfiling :: Server -> IO (Maybe CmdFailure)
cmdUnpauseProfiling s = helpCmd s "unpause_profiling" []

-- | @set_tuning_param param value@
cmdSetTuningParam :: Server -> Text -> Text -> IO (Either CmdFailure [Text])
cmdSetTuningParam s param value = sendCommand s "set_tuning_param" [param, value]

-- | @tuning_params@
cmdTuningParams :: Server -> Text -> IO (Either CmdFailure [Text])
cmdTuningParams s entry = sendCommand s "tuning_params" [entry]

-- | @tuning_param_class param@
cmdTuningParamClass :: Server -> Text -> IO (Either CmdFailure Text)
cmdTuningParamClass s param = fmap head <$> sendCommand s "tuning_param_class" [param]

-- | @types@
cmdTypes :: Server -> IO (Either CmdFailure [Text])
cmdTypes s = sendCommand s "types" []

-- | @entry_points@
cmdEntryPoints :: Server -> IO (Either CmdFailure [Text])
cmdEntryPoints s = sendCommand s "entry_points" []

-- | @fields type@
cmdFields :: Server -> Text -> IO (Either CmdFailure [Text])
cmdFields s t = sendCommand s "fields" [t]

-- | @new var0 type var1...@
cmdNew :: Server -> Text -> Text -> [Text] -> IO (Maybe CmdFailure)
cmdNew s var0 t vars = helpCmd s "new" $ var0 : t : vars

-- | @project to from field@
cmdProject :: Server -> Text -> Text -> Text -> IO (Maybe CmdFailure)
cmdProject s to from field = helpCmd s "project" [to, from, field]

-- | Turn a 'Maybe'-producing command into a monadic action.
cmdMaybe :: (MonadError Text m, MonadIO m) => IO (Maybe CmdFailure) -> m ()
cmdMaybe = maybe (pure ()) (throwError . T.unlines . failureMsg) <=< liftIO

-- | Turn an 'Either'-producing command into a monadic action.
cmdEither :: (MonadError Text m, MonadIO m) => IO (Either CmdFailure a) -> m a
cmdEither = either (throwError . T.unlines . failureMsg) pure <=< liftIO
