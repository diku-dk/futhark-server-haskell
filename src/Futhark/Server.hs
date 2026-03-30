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
-- t'CmdFailure'.  However, certain errors (such as if the server
-- process terminates unexpectedly, or temporary files cannot be
-- created) will result in an IO exception.
--
-- Many of the functions here are documented only as the server
-- protocol command they correspond to.  See the protocol
-- documentation for details.
--
-- Type aliases are added for readability, but many of the commands simply
-- accept and produce 'T.Text's.
module Futhark.Server
  ( -- * Server creation
    Server,
    ServerCfg (..),
    newServerCfg,
    withServer,
    ServerException (..),

    -- * Commands
    Cmd,
    CmdFailure (..),
    FieldName,
    VariantName,
    VarName,
    TypeName,
    EntryName,
    TuningParamName,
    Kind (..),
    Variant (..),
    Field (..),
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
    cmdKind,
    cmdType,

    -- * Arrays
    cmdNewArray,
    cmdRank,
    cmdElemtype,
    cmdShape,
    cmdSet,
    cmdIndex,

    -- ** Records
    cmdNew,
    cmdProject,
    cmdFields,

    -- ** Sums
    cmdVariants,
    cmdConstruct,
    cmdDestruct,
    cmdVariant,

    -- ** Auxiliary
    cmdReport,
    cmdPauseProfiling,
    cmdUnpauseProfiling,
    cmdSetTuningParam,
    cmdTuningParams,
    cmdTuningParamClass,
    cmdAttributes,

    -- * Utility
    cmdMaybe,
    cmdEither,

    -- * Raw
    startServer,
    stopServer,
    abortServer,
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

-- | An unexpected IO exception describing an error related to interacting with
-- the server, such as failing to start it, or the server crashing.
data ServerException
  = -- | Human-readable error message.
    ServerException T.Text
  deriving (Eq, Ord)

instance Exception ServerException

instance Show ServerException where
  show (ServerException s) = T.unpack s

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
    -- | A function that is invoked on every line of output sent by the
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

-- | Start up a server. Make sure that 'stopServer' is eventually called on the
-- server. If this does not happen, then temporary files may be left on the file
-- system. You almost certainly wish to use 'bracket' or similar to avoid this.
-- Throws 'ServerException' if startup fails.
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
      throw . ServerException $
        "Cannot start " <> T.pack prog <> ": error " <> T.pack (show e)
    Just ExitSuccess ->
      throw . ServerException $
        "Cannot start " <> T.pack prog <> ": terminated immediately, but reported success."
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
      stderr_s <- T.readFile $ serverErrLog s
      removeFile $ serverErrLog s
      throw . ServerException $
        "Command failed with "
          <> T.pack (show code)
          <> ":\n"
          <> T.pack (unwords (prog : options))
          <> "\nStderr:\n"
          <> stderr_s

-- | Shut down a server. It may not be used again. Throws 'ServerException' if
-- the server process terminates with a failing exit code (i.e. anything but
-- 'ExitSuccess').
stopServer :: Server -> IO ()
stopServer s = flip finally (removeFile (serverErrLog s)) $ do
  hClose $ serverStdin s
  code <- P.waitForProcess $ serverProc s
  case code of
    ExitSuccess -> pure ()
    ExitFailure x -> do
      stderr_s <- T.readFile $ serverErrLog s
      throw . ServerException $
        "Server terminated with nonzero exit code "
          <> T.pack (show x)
          <> " and stderr:\n"
          <> stderr_s

-- | Terminate the server process.  You'll still need to call
-- 'stopServer' unless used inside 'withServer', which does it for
-- you.
abortServer :: Server -> IO ()
abortServer = P.terminateProcess . serverProc

-- | Start a server, execute an action, then shut down the server.
-- The t'Server' may not be returned from the action.
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
                "\nServer process exited unexpectedly with exit code: " <> T.pack (show x)
              Just ExitSuccess -> mempty
              Nothing -> mempty
      stderr_s <- T.readFile $ serverErrLog s
      throw . ServerException $
        "After sending command "
          <> cmd
          <> " to server process:"
          <> T.pack (show e)
          <> code_msg
          <> "\nServer stderr:\n"
          <> stderr_s

-- | The name of a server-side variable.
type VarName = Text

-- | The name of a server-side type.
type TypeName = Text

-- | The name of an entry point.
type EntryName = Text

-- | The name of a tuning parameter.
type TuningParamName = Text

-- | The name of a field.
type FieldName = Text

-- | The name of a variant.
type VariantName = Text

data Kind
  = Primitive
  | Array
  | Record
  | Sum
  | Opaque
  deriving (Eq, Ord, Show)

-- | A record field
data Field = Field
  { fieldName :: FieldName,
    fieldType :: TypeName
  }
  deriving (Eq, Ord, Show)

-- | A sum variant
data Variant = Variant
  { variantName :: VariantName,
    variantTypes :: [TypeName]
  }
  deriving (Eq, Ord, Show)

-- | The type of an input of an entry point.  If 'inputConsumed', then
-- the value passed in a 'cmdCall' must not be used again (nor any of
-- its aliases).
data InputType = InputType
  { inputConsumed :: Bool,
    inputType :: TypeName
  }
  deriving (Eq, Ord, Show)

-- | The type of an output of an entry point.  If 'outputUnique', then
-- the value returned does not alias any of the inputs.  See the
-- Futhark language manual itself for more details - the implications
-- are quite subtle (but you can ignore them unless you manually use
-- type annotations to make some entry point parameters unique).
data OutputType = OutputType
  { outputUnique :: Bool,
    outputType :: TypeName
  }
  deriving (Eq, Ord, Show)

inOutType :: (Bool -> TypeName -> a) -> Text -> a
inOutType f t =
  case T.uncons t of
    Just ('*', t') -> f True t'
    Just _ -> f False t
    Nothing -> f False t

helpCmd :: Server -> Cmd -> [Text] -> IO (Maybe CmdFailure)
helpCmd s cmd args =
  either Just (const Nothing) <$> sendCommand s cmd args

sendCommandSL :: Server -> Cmd -> [Text] -> IO (Either CmdFailure Text)
sendCommandSL s cmd args = fmap expectSingleLine <$> sendCommand s cmd args
  where
    expectSingleLine (a : _) = a
    expectSingleLine [] = error $ "Expected output of command \"" ++ T.unpack (T.intercalate " " $ cmd : args) ++ "\""

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
cmdSetTuningParam :: Server -> TuningParamName -> Int -> IO (Maybe CmdFailure)
cmdSetTuningParam s param value = helpCmd s "set_tuning_param" [param, T.pack (show value)]

-- | @tuning_params entry_point@
cmdTuningParams :: Server -> EntryName -> IO (Either CmdFailure [TuningParamName])
cmdTuningParams s entry = sendCommand s "tuning_params" [entry]

-- | @tuning_param_class param@
cmdTuningParamClass :: Server -> TuningParamName -> IO (Either CmdFailure Text)
cmdTuningParamClass s param = fmap mconcat <$> sendCommand s "tuning_param_class" [param]

-- | @attributes entry_point@
cmdAttributes :: Server -> EntryName -> IO (Either CmdFailure [Text])
cmdAttributes s entry = sendCommand s "attributes" [entry]

-- | @types@
cmdTypes :: Server -> IO (Either CmdFailure [TypeName])
cmdTypes s = sendCommand s "types" []

-- | @entry_points@
cmdEntryPoints :: Server -> IO (Either CmdFailure [EntryName])
cmdEntryPoints s = sendCommand s "entry_points" []

-- | @kind t@
cmdKind :: Server -> TypeName -> IO (Either CmdFailure Kind)
cmdKind s t = fmap parseKind <$> sendCommandSL s "kind" [t]
  where
    parseKind "primitive" = Primitive
    parseKind "array" = Array
    parseKind "record" = Record
    parseKind "sum" = Sum
    parseKind "opaque" = Opaque
    parseKind _ = error $ "Invalid kind of type \"" ++ T.unpack t ++ "\""

-- | @type v@
cmdType :: Server -> VarName -> IO (Either CmdFailure TypeName)
cmdType s v = sendCommandSL s "type" [v]

-- | @new_array v0 t s0 ... sN-1 v1 ... vM@
cmdNewArray :: Server -> VarName -> TypeName -> [Int] -> [VarName] -> IO (Maybe CmdFailure)
cmdNewArray s var0 t sizes vars = helpCmd s "new_array" $ var0 : t : map T.show sizes ++ vars

-- | @rank t@
cmdRank :: Server -> TypeName -> IO (Either CmdFailure Int)
cmdRank s t = fmap (read . T.unpack) <$> sendCommandSL s "rank" [t]

-- | @elemtype t@
cmdElemtype :: Server -> TypeName -> IO (Either CmdFailure TypeName)
cmdElemtype s t = sendCommandSL s "elemtype" [t]

-- | @shape v@
cmdShape :: Server -> VarName -> IO (Either CmdFailure [Int])
cmdShape s v = fmap (map (read . T.unpack) . T.words) <$> sendCommandSL s "shape" [v]

-- | @set v0 v1 i0 ... iN-1@
cmdSet :: Server -> VarName -> VarName -> [Int] -> IO (Maybe CmdFailure)
cmdSet s v0 v1 is =
  helpCmd s "set" $ [v0, v1] <> map T.show is

-- | @index v0 v1 i0 ... iN-1@
cmdIndex :: Server -> VarName -> VarName -> [Int] -> IO (Maybe CmdFailure)
cmdIndex s v0 v1 is =
  helpCmd s "index" $ [v0, v1] <> map T.show is

-- | @fields type@
cmdFields :: Server -> TypeName -> IO (Either CmdFailure [Field])
cmdFields s t = fmap (zipWith parseField [1 ..]) <$> sendCommand s "fields" [t]
  where
    parseField :: Int -> Text -> Field
    parseField l f =
      case T.words f of
        (fn : ft) -> Field fn $ T.unwords ft
        _ -> error $ "Invalid field on line " ++ show l ++ " of `fields` output: \"" ++ T.unpack f ++ "\""

-- | @new var0 type var1...@
cmdNew :: Server -> VarName -> TypeName -> [VarName] -> IO (Maybe CmdFailure)
cmdNew s var0 t vars = helpCmd s "new" $ var0 : t : vars

-- | @project to from field@
cmdProject :: Server -> VarName -> VarName -> Text -> IO (Maybe CmdFailure)
cmdProject s to from field = helpCmd s "project" [to, from, field]

-- | @variants t@
cmdVariants :: Server -> TypeName -> IO (Either CmdFailure [Variant])
cmdVariants s t = fmap parseVariants <$> sendCommand s "variants" [t]
  where
    parseVariants :: [Text] -> [Variant]
    parseVariants = map mkVariant . go []
      where
        go acc [] = [reverse acc]
        go [] (l : ls) = go [l] ls
        go acc (l : ls)
          | "- " `T.isPrefixOf` l = go (T.drop 2 l : acc) ls
          | otherwise = reverse acc : go [l] ls

    mkVariant :: [Text] -> Variant
    mkVariant (n : ts) = Variant n ts
    mkVariant [] = error $ "Invalid output of \"variants " ++ T.unpack t ++ "\""

-- | @construct var0 type variant var1...@
cmdConstruct :: Server -> VarName -> TypeName -> VariantName -> [VarName] -> IO (Maybe CmdFailure)
cmdConstruct s var0 t variant vars = helpCmd s "construct" $ var0 : t : variant : vars

-- | @destruct var0 var1...@
cmdDestruct :: Server -> VarName -> [VarName] -> IO (Maybe CmdFailure)
cmdDestruct s var0 vars = helpCmd s "destruct" $ var0 : vars

-- | @variant v@
cmdVariant :: Server -> VarName -> IO (Either CmdFailure VariantName)
cmdVariant s v = sendCommandSL s "variant" [v]

-- | Turn a 'Maybe'-producing command into a monadic action.
cmdMaybe :: (MonadError Text m, MonadIO m) => IO (Maybe CmdFailure) -> m ()
cmdMaybe = maybe (pure ()) (throwError . T.unlines . failureMsg) <=< liftIO

-- | Turn an 'Either'-producing command into a monadic action.
cmdEither :: (MonadError Text m, MonadIO m) => IO (Either CmdFailure a) -> m a
cmdEither = either (throwError . T.unlines . failureMsg) pure <=< liftIO
