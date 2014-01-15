{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------
-- |
-- License   :  BSD3
-- Stability :  experimental
-- Portability: type-families, generalized newtype deriving
--
-- This module provides a 'VarInt' wrapper with a 'Serial' instance
-- that generates base-128 variable-width ints.  Values are encoded 7
-- bits at a time, with the most significant being a continuation bit.
-- Thus, the numbers from 0 to 127 require only a single byte to
-- encode, those from 128 to 16383 require two bytes, etc.
--
-- This format is taken from Google's /Protocol Buffers/, which
-- provides a bit more verbiage on the encoding:
-- <https://developers.google.com/protocol-buffers/docs/encoding#varints>.
--------------------------------------------------------------------

module Data.Bytes.VarInt
  ( VarInt(..)
  ) where

import Data.Bits
import Data.Bytes.Signed

newtype VarInt n = VarInt { unVarInt :: n }
  deriving (Eq, Ord, Show, Enum, Num, Integral, Bounded, Real, Bits)

type instance Unsigned (VarInt n) = VarInt (Unsigned n)
type instance Signed (VarInt n) = VarInt (Signed n)

