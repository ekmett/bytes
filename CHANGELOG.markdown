0.17.3 [2023.08.06]
-------------------
* Remove `mtl` dependency in favor `transformers`, as `bytes` was only using
  `mtl` for its `transformers` re-exports.

0.17.2 [2022.05.07]
-------------------
* Allow building with `mtl-2.3.*` and `transformers-0.6.*`.

0.17.1 [2021.02.17]
-------------------
* The build-type has been changed from `Custom` to `Simple`.
  To achieve this, the `doctests` test suite has been removed in favor of using
  [`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec)
  to run the doctests.
* Provide the `Serial Natural` instance unconditionally.
* Allow building with `bytestring-0.11.*`.

0.17 [2020.02.03]
-----------------
* Give `MonadGet m` a superclass of
  `forall a b. Coercible a b => Coercible (m a) (m b)` when built against
  GHC 8.6 or later. This allows `Serial` instances to be derived using
  `GeneralizedNewtypeDeriving` or `DerivingVia` when using in tandem with
  `StandaloneDeriving`.

0.16 [2019.08.27]
-----------------
* Support GHC-8.8.
* `MonadGet` now requires `MonadFail` as a superclass.

0.15.5 [2018.07.03]
-------------------
* Add `Serial(1)` instances for `NonEmpty`.

0.15.4 [2018.04.05]
-------------------
* Use a significantly simpler `Setup.hs` script.

0.15.3
------
* Support GHC 8.2
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-2.0`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.

0.15.2
------
* Support ghc 8
* Support `time` 1.6
* Support `binary` 0.8
* Support `transformers` 0.5

0.15.1
------
* Drop `Trustworthy` claim in `Data.Bytes.Put` as we now can sometimes infer `Safe`.
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
