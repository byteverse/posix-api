# posix-api

## Objective

This library provides minimal bindings to system calls for POSIX-compliant
operating systems. All functions follow these design decisions:

* `String` is not used anywhere. `ByteArray` (from `primitive`) is used for
  serialized data. It is also used in certain filesystem function variants
  used in contexts where the paths are only ever handed over to other
  filesystem functions. `Addr` (from `primitive`) is used for pointers to
  data whose type is unknown. `Ptr` is used for pointers to data whose type
  is known.
* Functions should not throw errors. This library uses `IO (Either Errno a)`
  in places where some libraries would use `IO a`.
* The numeric types from `Foreign.C.Types` and `System.Posix.Types` are
  used in the type signatures of functions so that a haskell function's
  type signature matches its underlying POSIX equivalent exactly.
* Flags are newtypes over `CInt` (or whatever integral type matches the
  posix specification) rather than enumerations. The data constructors
  are exported, making the types extensible for operating system that
  have additional flags.
* There is some platform-specific code in this library. POSIX-specified data
  structures do not have the same in-memory representation on all platforms.
  Consequently, some of the code to serialize data to its C-struct
  representation must be written differently on different platforms.
  This is seldom needed. A viable alternative would be using the FFI
  to perform this serialization. However, the approach of using
  per-platform haskell code lets the serialization code inline better.

Pull requests that add bindings to POSIX APIs in a way that agrees
with these guidelines will be accepted. Unfortunately, there is some
grey area when it comes to what a "minimal binding" to a function
is. Discussion may sometimes be necessary to refine the guidelines.

## Infelicities

This project currently includes some Linux-specific code. It in the
the `Linux.Socket`. The plan is to eventually move the `Linux.Socket` module
into its own library. Currently, a ton of POSIX APIs are missing.
These should be included.

