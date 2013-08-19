{-# LANGUAGE FlexibleContexts #-}
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
import Data.Bytes.Unsigned
import Data.Word

-- | Integer/Word types serialized to base-128 variable-width ints.
--
-- >>> runPutL $ serialize (97 :: Word64)
-- "a\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
-- >>> runPutL $ serialize (97 :: VarInt Word64)
-- "a"
newtype VarInt n = VarInt {
      unVarInt :: n
    } deriving (Eq, Ord, Show, Enum, Num, Integral, Bounded, Real, Bits)

instance Unsigned n => Unsigned (VarInt n) where
    type Signless (VarInt n) = VarInt (Signless n)
    unsign (VarInt n) = VarInt (unsign n)

instance (Unsigned n, Bits n) => Serial (VarInt n) where
    serialize (VarInt n) = putVarInt $ unsign n
    {-# INLINE serialize #-}
    deserialize = getWord8 >>= getVarInt
    {-# INLINE deserialize #-}

putVarInt :: (Integral a, Bits a, MonadPut m) => a -> m ()
putVarInt n | n < 0x80  = putWord8 $ fromIntegral n
            | otherwise = do putWord8 $  fromIntegral n `setBit` 7
                             putVarInt $ n `shiftR` 7
{-# INLINE putVarInt #-}
getVarInt :: (MonadGet m, Bits b, Unsigned b) => Word8 -> m (VarInt b)
getVarInt n | n `testBit` 7 = do VarInt m <- deserialize
                                 return $ (m `shiftL` 7)
                                          .|. (fromIntegral n `clearBit` 7)
            | otherwise     = return . fromIntegral $ n
{-# INLINE getVarInt #-}
