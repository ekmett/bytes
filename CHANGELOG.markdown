0.15.0.2
--------
* Bump `cereal` bound for 0.5.0.0
* Add instance `Serial Natural`

0.15.0.1
--------
* Updated github URLs in the .cabal file.
* We now compile without warnings on GHC 7.10.

0.15
----
* Fixed a serious bug in the semantics of generic `Serial1` generation for the recursive case and improved `Generic1` support for `:.:`.

0.14.1.2
--------
* Support `void` 0.7 / GHC 7.10

0.14
----
* Lots of new instances
* `text` bound bump to allow 1.1.

0.13.0.1
--------
* Bumped dependency on `text` to support 1.0

0.13
----
* Fixed an issue caused by [deserializing illegal maps](http://www.reddit.com/r/haskell/comments/1q4r3b/mindbending_behavior_for_deserialization_in/).

0.11.5
------
* Fixed issue #7, permitting the doctests to function against bytestring 0.9

0.11.4
------
* Fixed issue #6 with regards to the test harness performance.

0.11.3
------
* Fixed the doctests from 0.11.2

0.11.2
------
* Constraint `binary` version for issue #5.

0.11.1
------
* Liberalized containers dependency to allow `containers` versions all the way back to 0.3 for stackage purposes

0.11
----
* Added `Data.Bytes.VarInt` and `Data.Bytes.Signed`.

0.10.2
------
* Switched to <stdint.h> to get more portable size correctness.

0.10.1
------
* Fixed typo in `cbits/i2d.c` that was causing a linking error.

0.10
----
* Changed all of the byte orders to big-endian by default *except* for `Word` and `Int`, which are variable sized.

0.9
-----
* Added proper support for `binary` 0.7.
* Restored `lookAheadM` and `lookAheadE`, thanks to the return of `lookAheadE` in `binary` 0.7.
* Renamed `Unchecked` to `Remaining`, and removed the `uncheckedLookAhead` function, as it is no longer supported downstream.

0.8
-----
* Trustworthiness

0.4
---
* Added a missing () instance

0.3
-----
* Added `Serial2` and various missing `Serial1` instances.

0.2
---
* Added `Serial` and `Serial1`.

0.1
---
* Repository initialized
