{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------
-- |
-- License   :  BSD3
-- Stability :  experimental
-- Portability: type-families
--
-- When one wants to think of an 'Int' as a dumb bitstring, converting
-- it to a 'Word' avoids pesky complications with respect to sign
-- extension.
--------------------------------------------------------------------

module Data.Bytes.Signed
  ( Unsigned, unsigned
  , Signed, signed
  ) where

import Data.Int
import Data.Word

type family Unsigned i :: *

type instance Unsigned Int = Word
type instance Unsigned Int8 = Word8
type instance Unsigned Int16 = Word16
type instance Unsigned Int32 = Word32
type instance Unsigned Int64 = Word64
type instance Unsigned Integer = Integer

type instance Unsigned Word = Word
type instance Unsigned Word8 = Word8
type instance Unsigned Word16 = Word16
type instance Unsigned Word32 = Word32
type instance Unsigned Word64 = Word64

unsigned :: (Integral i, Num (Unsigned i)) => i -> Unsigned i
unsigned = fromIntegral

type family Signed i :: *
type instance Signed Int     = Int
type instance Signed Int8    = Int8
type instance Signed Int16   = Int16
type instance Signed Int32   = Int32
type instance Signed Int64   = Int64
type instance Signed Integer = Integer

type instance Signed Word   = Int
type instance Signed Word8  = Int8
type instance Signed Word16 = Int16
type instance Signed Word32 = Int32
type instance Signed Word64 = Int64

signed :: (Integral i, Num (Signed i)) => i -> Signed i
signed = fromIntegral
