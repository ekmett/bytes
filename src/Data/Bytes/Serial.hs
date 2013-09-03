{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Trustworthy #-}
#endif
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module contains four classes, each providing methods to
-- serialize and deserialize types. 'Serial' is the main class, to
-- be used if there is a single, canonical way to serialize a
-- specific type. 'SerialBE' and 'SerialLE' in turn are used to
-- respectively implement big endian and little endian
-- serializations.
--------------------------------------------------------------------
module Data.Bytes.Serial
  ( SerialBE(..)
  , SerialLE(..)
  , SerialHost(..)
  , Serial(..)
  , GSerial(..)
  , Serial1(..), serialize1, deserialize1
  , GSerial1(..)
  , Serial2(..), serialize2, deserialize2
  , store, restore
  ) where

import Control.Monad
import qualified Data.Foldable as F
import Data.Bytes.Get
import Data.Bytes.Put
import Data.ByteString.Internal
import Data.ByteString.Lazy as Lazy
import Data.ByteString as Strict
import Data.Int
import qualified Data.IntMap as IMap
import qualified Data.IntSet as ISet
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text as SText
import Data.Text.Encoding as SText
import Data.Text.Lazy as LText
import Data.Text.Lazy.Encoding as LText
import Data.Void
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import System.IO.Unsafe

foreign import ccall floatToWord32 :: Float -> Word32
foreign import ccall word32ToFloat :: Word32 -> Float
foreign import ccall doubleToWord64 :: Double -> Word64
foreign import ccall word64ToDouble :: Word64 -> Double

------------------------------------------------------------------------------
-- Endianness-Dependant Serialization
------------------------------------------------------------------------------

{-| Methods to serialize and deserialize type 'a' to a big endian binary
representation
-}
class SerialBE a where
  serializeBE :: MonadPut m => a -> m ()
  deserializeBE :: MonadGet m => m a

instance SerialBE Double where
  serializeBE = serializeBE . doubleToWord64
  deserializeBE = liftM word64ToDouble deserializeBE

instance SerialBE Float where
  serializeBE = serializeBE . floatToWord32
  deserializeBE = liftM word32ToFloat deserializeBE

instance SerialBE Char where
  serializeBE = putWord32be . fromIntegral . fromEnum
  deserializeBE = liftM (toEnum . fromIntegral) getWord32be

instance SerialBE Word64 where
  serializeBE = putWord64be
  deserializeBE = getWord64be

instance SerialBE Word32 where
  serializeBE = putWord32be
  deserializeBE = getWord32be

instance SerialBE Word16 where
  serializeBE = putWord16be
  deserializeBE = getWord16be

instance SerialBE Int64 where
  serializeBE = putWord64be . fromIntegral
  deserializeBE = liftM fromIntegral getWord64be

instance SerialBE Int32 where
  serializeBE = putWord32be . fromIntegral
  deserializeBE = liftM fromIntegral getWord32be

instance SerialBE Int16 where
  serializeBE = putWord16be . fromIntegral
  deserializeBE = liftM fromIntegral getWord16be


{-| Methods to serialize and deserialize type 'a' to a little endian binary
representation
-}
class SerialLE a where
  serializeLE :: MonadPut m => a -> m ()
  deserializeLE :: MonadGet m => m a

instance SerialLE Double where
  serializeLE = serializeLE . doubleToWord64
  deserializeLE = liftM word64ToDouble deserializeLE

instance SerialLE Float where
  serializeLE = serializeLE . floatToWord32
  deserializeLE = liftM word32ToFloat deserializeLE

instance SerialLE Char where
  serializeLE = putWord32le . fromIntegral . fromEnum
  deserializeLE = liftM (toEnum . fromIntegral) getWord32le

instance SerialLE Word64 where
  serializeLE = putWord64le
  deserializeLE = getWord64le

instance SerialLE Word32 where
  serializeLE = putWord32le
  deserializeLE = getWord32le

instance SerialLE Word16 where
  serializeLE = putWord16le
  deserializeLE = getWord16le

instance SerialLE Int64 where
  serializeLE = putWord64le . fromIntegral
  deserializeLE = liftM fromIntegral getWord64le

instance SerialLE Int32 where
  serializeLE = putWord32le . fromIntegral
  deserializeLE = liftM fromIntegral getWord32le

instance SerialLE Int16 where
  serializeLE = putWord16le . fromIntegral
  deserializeLE = liftM fromIntegral getWord16le


{-| Methods to serialize and deserialize type 'a' to a binary representation
with the same endianness as that native to the current machine.
-}
class SerialHost a where
  serializeHost :: MonadPut m => a -> m ()
  deserializeHost :: MonadGet m => m a

instance SerialHost Double where
  serializeHost = serializeHost . doubleToWord64
  deserializeHost = liftM word64ToDouble deserializeHost

instance SerialHost Float where
  serializeHost = serializeHost . floatToWord32
  deserializeHost = liftM word32ToFloat deserializeHost

instance SerialHost Char where
  serializeHost = putWord32host . fromIntegral . fromEnum
  deserializeHost = liftM (toEnum . fromIntegral) getWord32host

instance SerialHost Word where
  serializeHost = putWordhost
  deserializeHost = getWordhost

instance SerialHost Word64 where
  serializeHost = putWord64host
  deserializeHost = getWord64host

instance SerialHost Word32 where
  serializeHost = putWord32host
  deserializeHost = getWord32host

instance SerialHost Word16 where
  serializeHost = putWord16host
  deserializeHost = getWord16host

instance SerialHost Word8 where
  serializeHost = putWord8
  deserializeHost = getWord8

instance SerialHost Int where
  serializeHost = putWordhost . fromIntegral
  deserializeHost = liftM fromIntegral getWordhost

instance SerialHost Int64 where
  serializeHost = putWord64host . fromIntegral
  deserializeHost = liftM fromIntegral getWord64host

instance SerialHost Int32 where
  serializeHost = putWord32host . fromIntegral
  deserializeHost = liftM fromIntegral getWord32host

instance SerialHost Int16 where
  serializeHost = putWord16host . fromIntegral
  deserializeHost = liftM fromIntegral getWord16host

instance SerialHost Int8 where
  serializeHost = putWord8 . fromIntegral
  deserializeHost = liftM fromIntegral getWord8

------------------------------------------------------------------------------
-- Serialization
------------------------------------------------------------------------------

{-| Methods to serialize and deserialize type 'a' to a binary representation

Instances provided here for fixed-with Integers and Words are big endian.
Instances for ByteStrings store also the length of bytestring big endian.
Instances for Word and Int are host endian as they are machine-specific types.
-}
class Serial a where
  serialize :: MonadPut m => a -> m ()
#ifndef HLINT
  default serialize :: (MonadPut m, GSerial (Rep a), Generic a) => a -> m ()
  serialize = gserialize . from
#endif

  deserialize :: MonadGet m => m a
#ifndef HLINT
  default deserialize :: (MonadGet m, Generic a, GSerial (Rep a)) => m a
  deserialize = liftM to gdeserialize
#endif

instance Serial Strict.ByteString where
  serialize bs = putWord32be (fromIntegral (Strict.length bs)) >> putByteString bs
  deserialize = do
    n <- getWord32be
    getByteString (fromIntegral n)

instance Serial Lazy.ByteString where
  serialize bs = putWord64be (fromIntegral (Lazy.length bs)) >> putLazyByteString bs
  deserialize = do
    n <- getWord64be
    getLazyByteString (fromIntegral n)

instance Serial SText.Text where
  serialize = serialize . SText.encodeUtf8
  deserialize = SText.decodeUtf8 `fmap` deserialize

instance Serial LText.Text where
  serialize = serialize . LText.encodeUtf8
  deserialize = LText.decodeUtf8 `fmap` deserialize

instance Serial ()
instance Serial a => Serial [a]
instance Serial a => Serial (Maybe a)
instance (Serial a, Serial b) => Serial (Either a b)
instance (Serial a, Serial b) => Serial (a, b)
instance (Serial a, Serial b, Serial c) => Serial (a, b, c)
instance (Serial a, Serial b, Serial c, Serial d) => Serial (a, b, c, d)
instance (Serial a, Serial b, Serial c, Serial d, Serial e) => Serial (a, b, c, d, e)

instance Serial Bool

store :: (MonadPut m, Storable a) => a -> m ()
store a = putByteString bs
  where bs = unsafePerformIO $ create (sizeOf a) $ \ p -> poke (castPtr p) a

restore :: forall m a. (MonadGet m, Storable a) => m a
restore = do
  let required = sizeOf (undefined :: a)
  PS fp o n <- getByteString required
  unless (n >= required) $ fail "restore: Required more bytes"
  return $ unsafePerformIO $ withForeignPtr fp $ \p -> peekByteOff p o

instance Serial Double where
  serialize = serializeBE
  deserialize = deserializeBE

instance Serial Float where
  serialize = serializeBE
  deserialize = deserializeBE

instance Serial Char where
  serialize = serializeBE
  deserialize = deserializeBE

 -- no BE instance
instance Serial Word where
  serialize = serializeHost
  deserialize = deserializeHost

instance Serial Word64 where
  serialize = serializeBE
  deserialize = deserializeBE

instance Serial Word32 where
  serialize = serializeBE
  deserialize = deserializeBE

instance Serial Word16 where
  serialize = serializeBE
  deserialize = deserializeBE

instance Serial Word8 where
  serialize = putWord8
  deserialize = getWord8

 -- no BE instance
instance Serial Int where
  serialize = serializeHost
  deserialize = deserializeHost

instance Serial Int64 where
  serialize = serializeBE
  deserialize = deserializeBE

instance Serial Int32 where
  serialize = serializeBE
  deserialize = deserializeBE

instance Serial Int16 where
  serialize = serializeBE
  deserialize = deserializeBE

instance Serial Int8 where
  serialize = putWord8 . fromIntegral
  deserialize = liftM fromIntegral getWord8

instance Serial Void where
  serialize = absurd
  deserialize = fail "I looked into the void."

instance Serial ISet.IntSet where
  serialize = serialize . ISet.toAscList
  deserialize = ISet.fromDistinctAscList `liftM` deserialize

instance Serial a => Serial (Seq.Seq a) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance Serial a => Serial (Set.Set a) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance Serial v => Serial (IMap.IntMap v) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

instance (Serial k, Serial v) => Serial (Map.Map k v) where
  serialize = serializeWith serialize
  deserialize = deserializeWith deserialize

------------------------------------------------------------------------------
-- Generic Serialization
------------------------------------------------------------------------------

-- | Used internally to provide generic serialization
class GSerial f where
  gserialize :: MonadPut m => f a -> m ()
  gdeserialize :: MonadGet m => m (f a)

instance GSerial U1 where
  gserialize U1 = return ()
  gdeserialize = return U1

instance GSerial V1 where
  gserialize _ = fail "I looked into the void."
  gdeserialize = fail "I looked into the void."

instance (GSerial f, GSerial g) => GSerial (f :*: g) where
  gserialize (f :*: g) = do
    gserialize f
    gserialize g
  gdeserialize = liftM2 (:*:) gdeserialize gdeserialize

instance (GSerial f, GSerial g) => GSerial (f :+: g) where
  gserialize (L1 x) = putWord8 0 >> gserialize x
  gserialize (R1 y) = putWord8 1 >> gserialize y
  gdeserialize = getWord8 >>= \a -> case a of
    0 -> liftM L1 gdeserialize
    1 -> liftM R1 gdeserialize
    _ -> fail "Missing case"

instance GSerial f => GSerial (M1 i c f) where
  gserialize (M1 x) = gserialize x
  gdeserialize = liftM M1 gdeserialize

instance Serial a => GSerial (K1 i a) where
  gserialize (K1 x) = serialize x
  gdeserialize = liftM K1 deserialize

------------------------------------------------------------------------------
-- Higher-Rank Serialization
------------------------------------------------------------------------------

class Serial1 f where
  serializeWith :: MonadPut m => (a -> m ()) -> f a -> m ()
#ifndef HLINT
  default serializeWith :: (MonadPut m, GSerial1 (Rep1 f), Generic1 f) => (a -> m ()) -> f a -> m ()
  serializeWith f = gserializeWith f . from1
#endif

  deserializeWith :: MonadGet m => m a -> m (f a)
#ifndef HLINT
  default deserializeWith :: (MonadGet m, GSerial1 (Rep1 f), Generic1 f) => m a -> m (f a)
  deserializeWith f = liftM to1 (gdeserializeWith f)
#endif

instance Serial1 [] where
  serializeWith _ [] = putWord8 0
  serializeWith f (x:xs) = putWord8 1 >> f x >> serializeWith f xs
  deserializeWith m = getWord8 >>= \a -> case a of
    0 -> return []
    1 -> liftM2 (:) m (deserializeWith m)
    _ -> error "[].deserializeWith: Missing case"
instance Serial1 Maybe where
  serializeWith _ Nothing = putWord8 0
  serializeWith f (Just a) = putWord8 1 >> f a
  deserializeWith m = getWord8 >>= \a -> case a of
    0 -> return Nothing
    1 -> liftM Just m
    _ -> error "Maybe.deserializeWith: Missing case"
instance Serial a => Serial1 (Either a) where
  serializeWith = serializeWith2 serialize
  deserializeWith = deserializeWith2 deserialize
instance Serial a => Serial1 ((,) a) where
  serializeWith = serializeWith2 serialize
  deserializeWith = deserializeWith2 deserialize
instance (Serial a, Serial b) => Serial1 ((,,) a b) where
  serializeWith = serializeWith2 serialize
  deserializeWith = deserializeWith2 deserialize
instance (Serial a, Serial b, Serial c) => Serial1 ((,,,) a b c) where
  serializeWith = serializeWith2 serialize
  deserializeWith = deserializeWith2 deserialize
instance (Serial a, Serial b, Serial c, Serial d) => Serial1 ((,,,,) a b c d) where
  serializeWith = serializeWith2 serialize
  deserializeWith = deserializeWith2 deserialize

instance Serial1 Seq.Seq where
  serializeWith pv = serializeWith pv . F.toList
  deserializeWith gv = Seq.fromList `liftM` deserializeWith gv

instance Serial1 Set.Set where
  serializeWith pv = serializeWith pv . Set.toAscList
  deserializeWith gv = Set.fromDistinctAscList `liftM` deserializeWith gv

instance Serial1 IMap.IntMap where
  serializeWith pv = serializeWith (serializeWith2 serialize pv)
                   . IMap.toAscList
  deserializeWith gv = IMap.fromDistinctAscList
               `liftM` deserializeWith (deserializeWith2 deserialize gv)

instance Serial k => Serial1 (Map.Map k) where
  serializeWith = serializeWith2 serialize
  deserializeWith = deserializeWith2 deserialize

serialize1 :: (MonadPut m, Serial1 f, Serial a) => f a -> m ()
serialize1 = serializeWith serialize
{-# INLINE serialize1 #-}

deserialize1 :: (MonadGet m, Serial1 f, Serial a) => m (f a)
deserialize1 = deserializeWith deserialize
{-# INLINE deserialize1 #-}

------------------------------------------------------------------------------
-- Higher-Rank Generic Serialization
------------------------------------------------------------------------------

-- | Used internally to provide generic serialization
class GSerial1 f where
  gserializeWith :: MonadPut m => (a -> m ()) -> f a -> m ()
  gdeserializeWith :: MonadGet m => m a -> m (f a)

instance GSerial1 Par1 where
  gserializeWith f (Par1 a) = f a
  gdeserializeWith m = liftM Par1 m

instance GSerial1 f => GSerial1 (Rec1 f) where
  gserializeWith f (Rec1 fa) = gserializeWith f fa
  gdeserializeWith m = liftM Rec1 (gdeserializeWith m)

-- instance (Serial1 f, GSerial1 g) => GSerial1 (f :.: g) where

instance GSerial1 U1 where
  gserializeWith _ U1 = return ()
  gdeserializeWith _  = return U1

instance GSerial1 V1 where
  gserializeWith _   = fail "I looked into the void."
  gdeserializeWith _ = fail "I looked into the void."

instance (GSerial1 f, GSerial1 g) => GSerial1 (f :*: g) where
  gserializeWith f (a :*: b) = gserializeWith f a >> gserializeWith f b
  gdeserializeWith m = liftM2 (:*:) (gdeserializeWith m) (gdeserializeWith m)

instance (GSerial1 f, GSerial1 g) => GSerial1 (f :+: g) where
  gserializeWith f (L1 x) = putWord8 0 >> gserializeWith f x
  gserializeWith f (R1 y) = putWord8 1 >> gserializeWith f y
  gdeserializeWith m = getWord8 >>= \a -> case a of
    0 -> liftM L1 (gdeserializeWith m)
    1 -> liftM R1 (gdeserializeWith m)
    _ -> fail "Missing case"

instance GSerial1 f => GSerial1 (M1 i c f) where
  gserializeWith f (M1 x) = gserializeWith f x
  gdeserializeWith = liftM M1 . gdeserializeWith

instance Serial a => GSerial1 (K1 i a) where
  gserializeWith _ (K1 x) = serialize x
  gdeserializeWith _ = liftM K1 deserialize

------------------------------------------------------------------------------
-- Higher-Rank Serialization
------------------------------------------------------------------------------

class Serial2 f where
  serializeWith2 :: MonadPut m => (a -> m ()) -> (b -> m ()) -> f a b -> m ()
  deserializeWith2 :: MonadGet m => m a -> m b ->  m (f a b)

serialize2 :: (MonadPut m, Serial2 f, Serial a, Serial b) => f a b -> m ()
serialize2 = serializeWith2 serialize serialize
{-# INLINE serialize2 #-}

deserialize2 :: (MonadGet m, Serial2 f, Serial a, Serial b) => m (f a b)
deserialize2 = deserializeWith2 deserialize deserialize
{-# INLINE deserialize2 #-}

instance Serial2 Either where
  serializeWith2 f _ (Left x)  = putWord8 0 >> f x
  serializeWith2 _ g (Right y) = putWord8 1 >> g y
  deserializeWith2 m n = getWord8 >>= \a -> case a of
    0 -> liftM Left m
    1 -> liftM Right n
    _ -> fail "Missing case"

instance Serial2 (,) where
  serializeWith2 f g (a, b) = f a >> g b
  deserializeWith2 m n = liftM2 (,) m n

instance Serial a => Serial2 ((,,) a) where
  serializeWith2 f g (a, b, c) = serialize a >> f b >> g c
  deserializeWith2 m n = liftM3 (,,) deserialize m n

instance (Serial a, Serial b) => Serial2 ((,,,) a b) where
  serializeWith2 f g (a, b, c, d) = serialize a >> serialize b >> f c >> g d
  deserializeWith2 m n = liftM4 (,,,) deserialize deserialize m n

instance (Serial a, Serial b, Serial c) => Serial2 ((,,,,) a b c) where
  serializeWith2 f g (a, b, c, d, e) = serialize a >> serialize b >> serialize c >> f d >> g e
  deserializeWith2 m n = liftM5 (,,,,) deserialize deserialize deserialize m n

instance Serial2 Map.Map where
  serializeWith2 pk pv = serializeWith (serializeWith2 pk pv) . Map.toAscList
  deserializeWith2 gk gv = Map.fromDistinctAscList
                   `liftM` deserializeWith (deserializeWith2 gk gv)
