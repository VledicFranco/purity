Change Log
==========

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