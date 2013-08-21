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
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Bytes.Signed
import Data.Word

-- | Integer/Word types serialized to base-128 variable-width ints.
--
-- >>> runPutL $ serialize (97 :: Word64)
-- "a\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
-- >>> runPutL $ serialize (97 :: VarInt Word64)
-- "a"
newtype VarInt n = VarInt { unVarInt :: n }
  deriving (Eq, Ord, Show, Enum, Num, Integral, Bounded, Real, Bits)

type instance Unsigned (VarInt n) = VarInt (Unsigned n)
type instance Signed (VarInt n) = VarInt (Signed n)

instance (Bits n, Integral n, Bits (Unsigned n), Integral (Unsigned n)) => Serial (VarInt n) where
  serialize (VarInt n) = putVarInt $ unsigned n
  {-# INLINE serialize #-}

  deserialize = getWord8 >>= getVarInt
  {-# INLINE deserialize #-}

putVarInt :: (MonadPut m, Integral a, Bits a) => a -> m ()
putVarInt n
  | n < 0x80 = putWord8 $ fromIntegral n
  | otherwise = do
    putWord8 $ setBit (fromIntegral n) 7
    putVarInt $ shiftR n 7
{-# INLINE putVarInt #-}

getVarInt :: (MonadGet m, Num b, Bits b) => Word8 -> m b
getVarInt n
  | testBit n 7 = do
    VarInt m <- getWord8 >>= getVarInt
    return $ shiftL m 7 .|. clearBit (fromIntegral n) 7
  | otherwise = return $ fromIntegral n
{-# INLINE getVarInt #-}
