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

module Data.Bytes.Unsigned
  ( Unsigned(..)
  ) where

import Data.Bits
import Data.Int
import Data.Word

-- | A type function to get the unsigned versions of int-ish things.
class (Integral (Signless i), Bits (Signless i), Integral i) => Unsigned i where
  type Signless i :: *
  unsign :: i -> Signless i
  -- | Default implementation is @unsign = fromIntegral@.
  unsign = fromIntegral
  {-# INLINE unsign #-}

instance Unsigned Word8 where
  type Signless Word8 = Word8
instance Unsigned Word16 where
  type Signless Word16 = Word16
instance Unsigned Word32 where
  type Signless Word32 = Word32
instance Unsigned Word64 where
  type Signless Word64 = Word64
instance Unsigned Word where
  type Signless Word = Word

instance Unsigned Int8 where
  type Signless Int8 = Word8
instance Unsigned Int16 where
  type Signless Int16 = Word16
instance Unsigned Int32 where
  type Signless Int32 = Word32
instance Unsigned Int64 where
  type Signless Int64 = Word64
instance Unsigned Int where
  type Signless Int = Word
