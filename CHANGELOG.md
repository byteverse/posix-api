# Changelog
All notable changes to this project will be documented in this file.

The format is inspired by [Keep a Changelog](http://keepachangelog.com/en/1.0.0/).
This changelog deviates from the recommendation by not grouping changes into
added, changed, deprecated, etc. subsections.

This project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.5.0.0] - 2023-07-13

- Move Linux.Systemd to systemd-api library to make docs build on hackage.

## [0.4.0.1] - 2023-06-27

- Build with GHC 9.4

## [0.4.0.0] - 2022-12-08

- Add `writeMutableByteArray`
- In the 0.3.5.0 release, the major version was supposed to be bumped.
  This is being done now instead.

## [0.3.5.0] - 2021-07-02

- Breaking: Start using pattern synonyms for macros.
- Add dedicated modules for peeking at structures.
- Make compatible with GHC 8.10 by changing the way ArrayArray# is handled
  on the C side of the FFI.
- Add `uninterruptibleSetSocketOption`.
- Add socket options `SO_BINDTODEVICE` and `SO_REUSEADDR`.

## [0.3.4.0] - 2020-03-09

- Add `Posix.File`
- Add lower bound for `hsc2hs` build tool

## [0.3.3.0] - 2019-12-18

- Support several POSIX message queue functions.
- Support Linux systemd functions.

## [0.3.2.0] - 2019-07-21

- Add more functions.

## [0.3.1.0] - YYYY-MM-DD

- Make the test suite build again.
- Add `uninterruptibleSendByteArrays`.

## [0.1.0.0] - 2018-01-02
- Initial release.
- Includes a ton of sockets API stuff.
- Includes the get working directory function.
