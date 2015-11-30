{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013-2015
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module generalizes the @binary@ 'B.PutM' and @cereal@ 'S.PutM'
-- monads in an ad hoc fashion to permit code to be written that is
-- compatible across them.
--
-- Moreover, this class permits code to be written to be portable over
-- various monad transformers applied to these as base monads.
--------------------------------------------------------------------
module Data.Bytes.Put
  ( MonadPut(..)
  , runPutL
  , runPutS
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Reader
import Control.Monad.Trans.Except as Except
import Control.Monad.RWS.Lazy as Lazy
import Control.Monad.RWS.Strict as Strict
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import Control.Monad.Writer.Lazy as Lazy
import Control.Monad.Writer.Strict as Strict
import qualified Data.Binary.Put as B
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import qualified Data.Serialize.Put as S
import Data.Word

------------------------------------------------------------------------------
-- MonadPut
------------------------------------------------------------------------------

class (Applicative m, Monad m) => MonadPut m where
  -- | Efficiently write a byte into the output buffer
  putWord8 :: Word8 -> m ()
#ifndef HLINT
  default putWord8 :: (m ~ t n, MonadTrans t, MonadPut n) => Word8 -> m ()
  putWord8 = lift . putWord8
  {-# INLINE putWord8 #-}
#endif

  -- | An efficient primitive to write a strict 'Strict.ByteString' into the output buffer.
  --
  -- In @binary@ this flushes the current buffer, and writes the argument into a new chunk.
  putByteString     :: Strict.ByteString -> m ()
#ifndef HLINT
  default putByteString :: (m ~ t n, MonadTrans t, MonadPut n) => Strict.ByteString -> m ()
  putByteString = lift . putByteString
  {-# INLINE putByteString #-}
#endif

  -- | Write a lazy 'Lazy.ByteString' efficiently.
  --
  -- With @binary@, this simply appends the chunks to the output buffer
  putLazyByteString :: Lazy.ByteString -> m ()
#ifndef HLINT
  default putLazyByteString :: (m ~ t n, MonadTrans t, MonadPut n) => Lazy.ByteString -> m ()
  putLazyByteString = lift . putLazyByteString
  {-# INLINE putLazyByteString #-}
#endif

  -- | Pop the 'ByteString' we have constructed so far, if any, yielding a
  -- new chunk in the result 'ByteString'.
  --
  -- If we're building a strict 'Strict.ByteString' with @cereal@ then this does nothing.
  flush :: m ()
#ifndef HLINT
  default flush :: (m ~ t n, MonadTrans t, MonadPut n) => m ()
  flush = lift flush
  {-# INLINE flush #-}
#endif

  -- | Write a 'Word16' in little endian format
  putWord16le   :: Word16 -> m ()
#ifndef HLINT
  default putWord16le :: (m ~ t n, MonadTrans t, MonadPut n) => Word16 -> m ()
  putWord16le = lift . putWord16le
  {-# INLINE putWord16le #-}
#endif

  -- | Write a 'Word16' in big endian format
  putWord16be   :: Word16 -> m ()
#ifndef HLINT
  default putWord16be :: (m ~ t n, MonadTrans t, MonadPut n) => Word16 -> m ()
  putWord16be = lift . putWord16be
  {-# INLINE putWord16be #-}
#endif

  -- | /O(1)./ Write a 'Word16' in native host order and host endianness.
  -- For portability issues see 'putWordhost'.
  putWord16host :: Word16 -> m ()
#ifndef HLINT
  default putWord16host :: (m ~ t n, MonadTrans t, MonadPut n) => Word16 -> m ()
  putWord16host = lift . putWord16host
  {-# INLINE putWord16host #-}
#endif

  -- | Write a 'Word32' in little endian format
  putWord32le   :: Word32 -> m ()
#ifndef HLINT
  default putWord32le :: (m ~ t n, MonadTrans t, MonadPut n) => Word32 -> m ()
  putWord32le = lift . putWord32le
  {-# INLINE putWord32le #-}
#endif

  -- | Write a 'Word32' in big endian format
  putWord32be   :: Word32 -> m ()
#ifndef HLINT
  default putWord32be :: (m ~ t n, MonadTrans t, MonadPut n) => Word32 -> m ()
  putWord32be = lift . putWord32be
  {-# INLINE putWord32be #-}
#endif

  -- | /O(1)./ Write a 'Word32' in native host order and host endianness.
  -- For portability issues see @putWordhost@.
  putWord32host :: Word32 -> m ()
#ifndef HLINT
  default putWord32host :: (m ~ t n, MonadTrans t, MonadPut n) => Word32 -> m ()
  putWord32host = lift . putWord32host
  {-# INLINE putWord32host #-}
#endif

  -- | Write a 'Word64' in little endian format
  putWord64le   :: Word64 -> m ()
#ifndef HLINT
  default putWord64le :: (m ~ t n, MonadTrans t, MonadPut n) => Word64 -> m ()
  putWord64le = lift . putWord64le
  {-# INLINE putWord64le #-}
#endif

  -- | Write a 'Word64' in big endian format
  putWord64be   :: Word64 -> m ()
#ifndef HLINT
  default putWord64be :: (m ~ t n, MonadTrans t, MonadPut n) => Word64 -> m ()
  putWord64be = lift . putWord64be
  {-# INLINE putWord64be #-}
#endif

  -- | /O(1)./ Write a 'Word64' in native host order and host endianness.
  -- For portability issues see @putWordhost@.
  putWord64host :: Word64 -> m ()
#ifndef HLINT
  default putWord64host :: (m ~ t n, MonadTrans t, MonadPut n) => Word64 -> m ()
  putWord64host = lift . putWord64host
  {-# INLINE putWord64host #-}
#endif


  -- | /O(1)./ Write a single native machine word. The word is
  -- written in host order, host endian form, for the machine you're on.
  -- On a 64 bit machine the Word is an 8 byte value, on a 32 bit machine,
  -- 4 bytes. Values written this way are not portable to
  -- different endian or word sized machines, without conversion.
  putWordhost :: Word -> m ()
#ifndef HLINT
  default putWordhost :: (m ~ t n, MonadTrans t, MonadPut n) => Word -> m ()
  putWordhost = lift . putWordhost
  {-# INLINE putWordhost #-}
#endif

instance MonadPut B.PutM where
  putWord8 = B.putWord8
  {-# INLINE putWord8 #-}
  putByteString = B.putByteString
  {-# INLINE putByteString #-}
  putLazyByteString = B.putLazyByteString
  {-# INLINE putLazyByteString #-}
  flush = B.flush
  {-# INLINE flush #-}
  putWord16le   = B.putWord16le
  {-# INLINE putWord16le #-}
  putWord16be   = B.putWord16be
  {-# INLINE putWord16be #-}
  putWord16host = B.putWord16host
  {-# INLINE putWord16host #-}
  putWord32le   = B.putWord32le
  {-# INLINE putWord32le #-}
  putWord32be   = B.putWord32be
  {-# INLINE putWord32be #-}
  putWord32host = B.putWord32host
  {-# INLINE putWord32host #-}
  putWord64le   = B.putWord64le
  {-# INLINE putWord64le #-}
  putWord64be   = B.putWord64be
  {-# INLINE putWord64be #-}
  putWord64host = B.putWord64host
  {-# INLINE putWord64host #-}
  putWordhost   = B.putWordhost
  {-# INLINE putWordhost #-}

instance MonadPut S.PutM where
  putWord8 = S.putWord8
  {-# INLINE putWord8 #-}
  putByteString = S.putByteString
  {-# INLINE putByteString #-}
  putLazyByteString = S.putLazyByteString
  {-# INLINE putLazyByteString #-}
  flush = S.flush
  {-# INLINE flush #-}
  putWord16le   = S.putWord16le
  {-# INLINE putWord16le #-}
  putWord16be   = S.putWord16be
  {-# INLINE putWord16be #-}
  putWord16host = S.putWord16host
  {-# INLINE putWord16host #-}
  putWord32le   = S.putWord32le
  {-# INLINE putWord32le #-}
  putWord32be   = S.putWord32be
  {-# INLINE putWord32be #-}
  putWord32host = S.putWord32host
  {-# INLINE putWord32host #-}
  putWord64le   = S.putWord64le
  {-# INLINE putWord64le #-}
  putWord64be   = S.putWord64be
  {-# INLINE putWord64be #-}
  putWord64host = S.putWord64host
  {-# INLINE putWord64host #-}
  putWordhost   = S.putWordhost
  {-# INLINE putWordhost #-}

instance MonadPut m => MonadPut (Lazy.StateT s m)
instance MonadPut m => MonadPut (Strict.StateT s m)
instance MonadPut m => MonadPut (ReaderT e m)
instance (MonadPut m, Monoid w) => MonadPut (Lazy.WriterT w m)
instance (MonadPut m, Monoid w) => MonadPut (Strict.WriterT w m)
instance (MonadPut m, Monoid w) => MonadPut (Lazy.RWST r w s m)
instance (MonadPut m, Monoid w) => MonadPut (Strict.RWST r w s m)
instance (MonadPut m) => MonadPut (ExceptT e m) where

-- | Put a value into a lazy 'Lazy.ByteString' using 'B.runPut'.
runPutL :: B.Put -> Lazy.ByteString
runPutL = B.runPut

-- | Put a value into a strict 'Strict.ByteString' using 'S.runPut'.
runPutS :: S.Put -> Strict.ByteString
runPutS = S.runPut
