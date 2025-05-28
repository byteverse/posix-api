# Changelog
All notable changes to this project will be documented in this file.

The format is inspired by [Keep a Changelog](http://keepachangelog.com/en/1.0.0/).
This changelog deviates from the recommendation by not grouping changes into
added, changed, deprecated, etc. subsections.

This project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.7.3.0 -- 2025-05-28

- Add support for `openat`

## 0.7.2.0 -- 2024-02-28

- Add `socket` as alias for `uninterruptibleSocket`.
- Add `withSocket`.
- Removed `UNLIFTEDARRAYFUNCTIONS` flag and support for GHC < 9.4.

## 0.7.1.0 -- 2023-10-03

- Add `uninterruptibleOpenModeUntypedFlags`.
- Add `Posix.Struct.AddressInfo.Poke`.
- Add `Posix.File.uninterruptibleReadMutableByteArray`.

## 0.7.0.0 -- 2023-08-30

- For now, remove all of the functions that work on UnliftedArray. These
  will be added back later once hackage starts using GHC 9.4. They are
  now guarded by CPP, so if anyone was using them, build this library
  with the `UNLIFTEDARRAYFUNCTIONS` flag to get them back.
- Add `uninterruptibleConnectPtr` for better compatibility with `network`.

## 0.6.1.0 -- 2023-08-14

- Add `uninterruptibleWriteBytesCompletelyErrno`
- Add `writeBytesCompletelyErrno`
- Add `uninterruptibleAccept4_`

## 0.6.0.1 -- 2023-07-13

- Fix mistake in header file that caused builds to fail

## 0.6.0.0 -- 2023-07-13

- Use Int instead of CInt for all offsets into byte arrays

## 0.5.0.0 -- 2023-07-13

- Move Linux.Systemd to systemd-api library to make docs build on hackage.

## 0.4.0.1 -- 2023-06-27

- Build with GHC 9.4

## 0.4.0.0 -- 2022-12-08

- Add `writeMutableByteArray`
- In the 0.3.5.0 release, the major version was supposed to be bumped.
  This is being done now instead.

## 0.3.5.0 -- 2021-07-02

- Breaking: Start using pattern synonyms for macros.
- Add dedicated modules for peeking at structures.
- Make compatible with GHC 8.10 by changing the way ArrayArray# is handled
  on the C side of the FFI.
- Add `uninterruptibleSetSocketOption`.
- Add socket options `SO_BINDTODEVICE` and `SO_REUSEADDR`.

## 0.3.4.0 -- 2020-03-09

- Add `Posix.File`
- Add lower bound for `hsc2hs` build tool

## 0.3.3.0 -- 2019-12-18

- Support several POSIX message queue functions.
- Support Linux systemd functions.

## 0.3.2.0 -- 2019-07-21

- Add more functions.

## 0.3.1.0 -- YYYY-MM-DD

- Make the test suite build again.
- Add `uninterruptibleSendByteArrays`.

## 0.1.0.0 -- 2018-01-02
- Initial release.
- Includes a ton of sockets API stuff.
- Includes the get working directory function.
