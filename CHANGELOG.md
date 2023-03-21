# Revision history for futhark-server

## 1.2.2.1 -- 2023-03-21

* Support GHC 9.6.

## 1.2.2.0 -- 2023-03-10

* Added `cmdTuningParams`.

* Fixed type of `cmdEntryPoints`.

## 1.2.1.0 -- 2022-07-01

* Added `cmdFields`, `cmdNew`, `cmdProject`, `cmdTypes`, and `cmdEntryPoints`.

## 1.2.0.0 -- 2022-05-14

* `ServerCfg` no longer has any type class instances.

* `ServerCfg` now has a `cfgOnLine` field.

## 1.1.2.1 -- 2022-02-03

* `withServer` no longer hides a previous exception if an exception
  occurs during `stopServer`.

## 1.1.2.0 -- 2021-10-24

* `stopServer` (and hence `withServer`) now throw an exception if the
  process fails.

## 1.1.1.0 -- 2021-09-30

* Added `cmdPauseProfiling`, `cmdUnpauseProfiling`, `cmdSetTuningParam`.

## 1.1.0.0 -- 2021-07-01

* `cmdInputs` and `cmdOutputs` now return `InputType` and `OutputType`
  values instead of just `TypeName`, in order to also capture
  consumption information.

## 1.0.0.0 -- 2021-06-17

* First version. Released on an unsuspecting world.
