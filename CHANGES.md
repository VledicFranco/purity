Change Log
==========

## v0.2.0

* Upgrades to cats v1.0.0
* Adds the concept of `Containers` which are simple traits (open product types) for dependencies of Script
* Adds a function "on" which accepts a `CantFail[ConfigContainer, Uri]` instead of just an Uri
* Log lines now contain information of the source file and line
* ColorPrint adds an optional timestamp and source file and line info

## v0.1.1

* Adds a first proposal of the http4s micro library.
* `Proposition[E, A]` now is a type class.
* `ScriptDSL[F[+_]]` renamed to `ScriptDsl[F[+_]]`.
* `ScriptDsl.raiseError` renamed to `ScriptDsl.raiseFailure`.
* Adds `ScriptDsl.raiseError`.
* Adds several useful type aliases inside `ScriptDsl[F[+_]]`
* `LoggerFunction` renamed to `Logger[F[+_]]`.
* Reimplements how logging is used within a `ScriptT` by directly composing a side effect using: `Logger[F[+_]]` .
* Add an instance of `Monoid` for `Logger[F]`.
* Addds several law checks for ScriptT and Logger.
* Several bug fixes.