# Revision history for recover-rtti

## 0.4.2 -- 2023-03-23

* Support mtl 2.3 (requiring at least 2.3.1) (#26).
  Thanks to Rebecca Turner.

## 0.4.1 -- 2022-03-17

* Support for ghc 9.2, bytestring 0.11, aeson 0.2
* Added `traceAnything` and `traceAnythingId` (#13)
* Added deriving-via support (`AnythingToString`, `BoxAnything`) (#3)

## 0.4 -- 2021-06-30

* Correctly set some required lower bounds.
* Add support for reclassification
* Add classification equality check
* Add support for primitive arrays and vectors
* Fix classification on OSX
* General internal cleanup of the library

This release is backwards incompatible with 0.3, but users that simply use
`anythingToString` should be unaffected.

## 0.3.0.0 -- 2021-03-17

* Fix bug that could cause `anythingToString` to fail on lists with an
  `elemUserDefined` exception (issue #6). Thanks to Bit Connor for the report!

## 0.2.1.0 -- 2021-03-17

* Add support for unordered-containers and boxed vectors.

## 0.2.0.0 -- 2021-03-15

* Reorganize module structure (primarily to improve Haddocks)

## 0.1.0.0 -- 2021-03-11

* Alpha release
