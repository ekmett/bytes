{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013-2015
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: type-families
--
-- This module generalizes the @binary@ 'B.Get' and @cereal@ 'S.Get'
-- monads in an ad hoc fashion to permit code to be written that is
-- compatible across them.
--
-- Moreover, this class permits code to be written to be portable over
-- various monad transformers applied to these as base monads.
--------------------------------------------------------------------
module Data.Bytes.Get
  ( MonadGet(..)
  , runGetL
  , runGetS
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
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Data.Int
import qualified Data.Serialize.Get as S
import Data.Word

import Control.Monad.Trans.Instances ()
import Data.Binary.Orphans ()
import qualified Control.Monad.Fail as Fail

class (Integral (Remaining m), Fail.MonadFail m, Applicative m) => MonadGet m where
  -- | An 'Integral' number type used for unchecked skips and counting.
  type Remaining m :: *

  -- | The underlying ByteString type used by this instance
  type Bytes m :: *

  -- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
  skip :: Int -> m ()
#ifndef HLINT
  default skip :: (MonadTrans t, MonadGet n, m ~ t n) => Int -> m ()
  skip = lift . skip
#endif

  -- | If at least @n@ bytes are available return at least that much of the current input.
  -- Otherwise fail.
  ensure :: Int -> m Strict.ByteString
#ifndef HLINT
  default ensure :: (MonadTrans t, MonadGet n, m ~ t n) => Int -> m Strict.ByteString
  ensure = lift . ensure
#endif

  -- | Run @ga@, but return without consuming its input.
  -- Fails if @ga@ fails.
  lookAhead :: m a -> m a

  -- | Like 'lookAhead', but consume the input if @gma@ returns 'Just _'.
  -- Fails if @gma@ fails.
  lookAheadM :: m (Maybe a) -> m (Maybe a)

  -- | Like 'lookAhead', but consume the input if @gea@ returns 'Right _'.
  -- Fails if @gea@ fails.
  lookAheadE :: m (Either a b) -> m (Either a b)

  -- | Pull @n@ bytes from the input, as a strict ByteString.
  getBytes :: Int -> m Strict.ByteString
#ifndef HLINT
  default getBytes :: (MonadTrans t, MonadGet n, m ~ t n) => Int -> m Strict.ByteString
  getBytes = lift . getBytes
#endif

  -- | Get the number of remaining unparsed bytes.
  -- Useful for checking whether all input has been consumed.
  -- Note that this forces the rest of the input.
  remaining :: m (Remaining m)
#ifndef HLINT
  default remaining :: (MonadTrans t, MonadGet n, m ~ t n, Remaining m ~ Remaining n)
                    => m (Remaining m)
  remaining = lift remaining
#endif

  -- | Test whether all input has been consumed,
  -- i.e. there are no remaining unparsed bytes.
  isEmpty :: m Bool
#ifndef HLINT
  default isEmpty :: (MonadTrans t, MonadGet n, m ~ t n) => m Bool
  isEmpty = lift isEmpty
#endif

  -- | Read a Word8 from the monad state
  getWord8 :: m Word8
#ifndef HLINT
  default getWord8 :: (MonadTrans t, MonadGet n, m ~ t n) => m Word8
  getWord8 = lift getWord8
#endif

  -- | An efficient 'get' method for strict ByteStrings. Fails if fewer
  -- than @n@ bytes are left in the input.
  getByteString :: Int -> m Strict.ByteString
#ifndef HLINT
  default getByteString :: (MonadTrans t, MonadGet n, m ~ t n) => Int -> m Strict.ByteString
  getByteString = lift . getByteString
#endif

  -- | An efficient 'get' method for lazy ByteStrings. Does not fail if fewer than
  -- @n@ bytes are left in the input.
  getLazyByteString :: Int64 -> m Lazy.ByteString
#ifndef HLINT
  default getLazyByteString :: (MonadTrans t, MonadGet n, m ~ t n) => Int64 -> m Lazy.ByteString
  getLazyByteString = lift . getLazyByteString
#endif

  -- | Read a 'Word16' in big endian format
  getWord16be   :: m Word16
#ifndef HLINT
  default getWord16be :: (MonadTrans t, MonadGet n, m ~ t n) => m Word16
  getWord16be = lift getWord16be
#endif

  -- | Read a 'Word16' in little endian format
  getWord16le   :: m Word16
#ifndef HLINT
  default getWord16le :: (MonadTrans t, MonadGet n, m ~ t n) => m Word16
  getWord16le = lift getWord16le
#endif

  -- | /O(1)./ Read a 2 byte 'Word16' in native host order and host endianness.
  getWord16host :: m Word16
#ifndef HLINT
  default getWord16host :: (MonadTrans t, MonadGet n, m ~ t n) => m Word16
  getWord16host = lift getWord16host
#endif

  -- | Read a 'Word32' in big endian format
  getWord32be   :: m Word32
#ifndef HLINT
  default getWord32be :: (MonadTrans t, MonadGet n, m ~ t n) => m Word32
  getWord32be = lift getWord32be
#endif

  -- | Read a 'Word32' in little endian format
  getWord32le   :: m Word32
#ifndef HLINT
  default getWord32le :: (MonadTrans t, MonadGet n, m ~ t n) => m Word32
  getWord32le = lift getWord32le
#endif

  -- | /O(1)./ Read a 'Word32' in native host order and host endianness.
  getWord32host :: m Word32
#ifndef HLINT
  default getWord32host :: (MonadTrans t, MonadGet n, m ~ t n) => m Word32
  getWord32host = lift getWord32host
#endif

  -- | Read a 'Word64' in big endian format
  getWord64be   :: m Word64
#ifndef HLINT
  default getWord64be :: (MonadTrans t, MonadGet n, m ~ t n) => m Word64
  getWord64be = lift getWord64be
#endif


  -- | Read a 'Word64' in little endian format
  getWord64le   :: m Word64
#ifndef HLINT
  default getWord64le :: (MonadTrans t, MonadGet n, m ~ t n) => m Word64
  getWord64le = lift getWord64le
#endif

  -- | /O(1)./ Read a 'Word64' in native host order and host endianness.
  getWord64host :: m Word64
#ifndef HLINT
  default getWord64host :: (MonadTrans t, MonadGet n, m ~ t n) => m Word64
  getWord64host = lift getWord64host
#endif

  -- | /O(1)./ Read a single native machine word. The word is read in
  -- host order, host endian form, for the machine you're on. On a 64 bit
  -- machine the Word is an 8 byte value, on a 32 bit machine, 4 bytes.
  getWordhost :: m Word
#ifndef HLINT
  default getWordhost :: (MonadTrans t, MonadGet n, m ~ t n) => m Word
  getWordhost = lift getWordhost
#endif

instance MonadGet B.Get where
  type Remaining B.Get = Int64
  type Bytes B.Get = Lazy.ByteString
  skip = B.skip
  {-# INLINE skip #-}
  lookAhead = B.lookAhead
  {-# INLINE lookAhead #-}
  lookAheadM = B.lookAheadM
  {-# INLINE lookAheadM #-}
  lookAheadE = B.lookAheadE
  {-# INLINE lookAheadE #-}
  ensure n = do
    bs <- lookAhead $ getByteString n
    unless (Strict.length bs >= n) $ Fail.fail "ensure: Required more bytes"
    return bs
  {-# INLINE ensure #-}
  getBytes = B.getByteString
  {-# INLINE getBytes #-}
  remaining = B.remaining
  {-# INLINE remaining #-}
  isEmpty = B.isEmpty
  {-# INLINE isEmpty #-}
  getWord8 = B.getWord8
  {-# INLINE getWord8 #-}
  getByteString = B.getByteString
  {-# INLINE getByteString #-}
  getLazyByteString = B.getLazyByteString
  {-# INLINE getLazyByteString #-}
  getWord16be   = B.getWord16be
  {-# INLINE getWord16be #-}
  getWord16le   = B.getWord16le
  {-# INLINE getWord16le #-}
  getWord16host = B.getWord16host
  {-# INLINE getWord16host #-}
  getWord32be   = B.getWord32be
  {-# INLINE getWord32be #-}
  getWord32le   = B.getWord32le
  {-# INLINE getWord32le #-}
  getWord32host = B.getWord32host
  {-# INLINE getWord32host #-}
  getWord64be   = B.getWord64be
  {-# INLINE getWord64be #-}
  getWord64le   = B.getWord64le
  {-# INLINE getWord64le #-}
  getWord64host = B.getWord64host
  {-# INLINE getWord64host #-}
  getWordhost   = B.getWordhost
  {-# INLINE getWordhost #-}

instance MonadGet S.Get where
  type Remaining S.Get = Int
  type Bytes S.Get = Strict.ByteString
  skip = S.skip
  {-# INLINE skip #-}
  lookAhead = S.lookAhead
  {-# INLINE lookAhead #-}
  lookAheadM = S.lookAheadM
  {-# INLINE lookAheadM #-}
  lookAheadE = S.lookAheadE
  {-# INLINE lookAheadE #-}
  getBytes = S.getBytes
  {-# INLINE getBytes #-}
  ensure = S.ensure
  {-# INLINE ensure #-}
  remaining = S.remaining
  {-# INLINE remaining #-}
  isEmpty = S.isEmpty
  {-# INLINE isEmpty #-}
  getWord8 = S.getWord8
  {-# INLINE getWord8 #-}
  getByteString = S.getByteString
  {-# INLINE getByteString #-}
  getLazyByteString = S.getLazyByteString
  {-# INLINE getLazyByteString #-}
  getWord16be   = S.getWord16be
  {-# INLINE getWord16be #-}
  getWord16le   = S.getWord16le
  {-# INLINE getWord16le #-}
  getWord16host = S.getWord16host
  {-# INLINE getWord16host #-}
  getWord32be   = S.getWord32be
  {-# INLINE getWord32be #-}
  getWord32le   = S.getWord32le
  {-# INLINE getWord32le #-}
  getWord32host = S.getWord32host
  {-# INLINE getWord32host #-}
  getWord64be   = S.getWord64be
  {-# INLINE getWord64be #-}
  getWord64le   = S.getWord64le
  {-# INLINE getWord64le #-}
  getWord64host = S.getWord64host
  {-# INLINE getWord64host #-}
  getWordhost   = S.getWordhost
  {-# INLINE getWordhost #-}

instance MonadGet m => MonadGet (Lazy.StateT s m) where
  type Remaining (Lazy.StateT s m) = Remaining m
  type Bytes (Lazy.StateT s m) = Bytes m
  lookAhead (Lazy.StateT m) = Lazy.StateT (lookAhead . m)
  {-# INLINE lookAhead #-}
  lookAheadM (Lazy.StateT m) = Lazy.StateT (liftM factor . lookAheadE . liftM distribute . m)
    where
    distribute (Nothing, s') = Left (Nothing, s')
    distribute (Just a, s') = Right (Just a, s')
    factor = either id id
  {-# INLINE lookAheadM #-}
  lookAheadE (Lazy.StateT m) = Lazy.StateT (liftM factor . lookAheadE . liftM distribute . m)
    where
    distribute (Left a, s') = Left (Left a, s')
    distribute (Right b, s') = Right (Right b, s')
    factor = either id id
  {-# INLINE lookAheadE #-}

instance MonadGet m => MonadGet (Strict.StateT s m) where
  type Remaining (Strict.StateT s m) = Remaining m
  type Bytes (Strict.StateT s m) = Bytes m
  lookAhead (Strict.StateT m) = Strict.StateT (lookAhead . m)
  {-# INLINE lookAhead #-}
  lookAheadM (Strict.StateT m) = Strict.StateT (liftM factor . lookAheadE . liftM distribute . m)
    where
    distribute (Nothing, s') = Left (Nothing, s')
    distribute (Just a, s') = Right (Just a, s')
    factor = either id id
  {-# INLINE lookAheadM #-}
  lookAheadE (Strict.StateT m) = Strict.StateT (liftM factor . lookAheadE . liftM distribute . m)
    where
    distribute (Left a, s') = Left (Left a, s')
    distribute (Right b, s') = Right (Right b, s')
    factor = either id id
  {-# INLINE lookAheadE #-}

instance MonadGet m => MonadGet (ReaderT e m) where
  type Remaining (ReaderT e m) = Remaining m
  type Bytes (ReaderT e m) = Bytes m
  lookAhead (ReaderT m) = ReaderT (lookAhead . m)
  {-# INLINE lookAhead #-}
  lookAheadM (ReaderT m) = ReaderT (lookAheadM . m)
  {-# INLINE lookAheadM #-}
  lookAheadE (ReaderT m) = ReaderT (lookAheadE . m)
  {-# INLINE lookAheadE #-}

instance (MonadGet m, Monoid w) => MonadGet (Lazy.WriterT w m) where
  type Remaining (Lazy.WriterT w m) = Remaining m
  type Bytes (Lazy.WriterT w m) = Bytes m
  lookAhead (Lazy.WriterT m) = Lazy.WriterT (lookAhead m)
  {-# INLINE lookAhead #-}
  lookAheadM (Lazy.WriterT m) = Lazy.WriterT (liftM factor $ lookAheadE $ liftM distribute m)
    where
    distribute (Nothing, s') = Left (Nothing, s')
    distribute (Just a, s') = Right (Just a, s')
    factor = either id id
  {-# INLINE lookAheadM #-}
  lookAheadE (Lazy.WriterT m) = Lazy.WriterT (liftM factor $ lookAheadE $ liftM distribute m)
    where
    distribute (Left a, s') = Left (Left a, s')
    distribute (Right b, s') = Right (Right b, s')
    factor = either id id
  {-# INLINE lookAheadE #-}

instance (MonadGet m, Monoid w) => MonadGet (Strict.WriterT w m) where
  type Remaining (Strict.WriterT w m) = Remaining m
  type Bytes (Strict.WriterT w m) = Bytes m
  lookAhead (Strict.WriterT m) = Strict.WriterT (lookAhead m)
  {-# INLINE lookAhead #-}
  lookAheadM (Strict.WriterT m) = Strict.WriterT (liftM factor $ lookAheadE $ liftM distribute m)
    where
    distribute (Nothing, s') = Left (Nothing, s')
    distribute (Just a, s') = Right (Just a, s')
    factor = either id id
  {-# INLINE lookAheadM #-}
  lookAheadE (Strict.WriterT m) = Strict.WriterT (liftM factor $ lookAheadE $ liftM distribute m)
    where
    distribute (Left a, s') = Left (Left a, s')
    distribute (Right b, s') = Right (Right b, s')
    factor = either id id
  {-# INLINE lookAheadE #-}

instance (MonadGet m, Monoid w) => MonadGet (Strict.RWST r w s m) where
  type Remaining (Strict.RWST r w s m) = Remaining m
  type Bytes (Strict.RWST r w s m) = Bytes m
  lookAhead (Strict.RWST m) = Strict.RWST $ \r s -> lookAhead (m r s)
  {-# INLINE lookAhead #-}
  lookAheadM (Strict.RWST m) = Strict.RWST (\r s -> liftM factor $ lookAheadE $ liftM distribute $ m r s )
    where
    distribute (Nothing, s',w') = Left (Nothing, s', w')
    distribute (Just a, s',w') = Right (Just a, s', w')
    factor = either id id
  {-# INLINE lookAheadM #-}
  lookAheadE (Strict.RWST m) = Strict.RWST (\r s -> liftM factor $ lookAheadE $ liftM distribute $ m r s)
    where
    distribute (Left a, s', w') = Left (Left a, s', w')
    distribute (Right b, s', w') = Right (Right b, s', w')
    factor = either id id
  {-# INLINE lookAheadE #-}

instance (MonadGet m, Monoid w) => MonadGet (Lazy.RWST r w s m) where
  type Remaining (Lazy.RWST r w s m) = Remaining m
  type Bytes (Lazy.RWST r w s m) = Bytes m
  lookAhead (Lazy.RWST m) = Lazy.RWST $ \r s -> lookAhead (m r s)
  {-# INLINE lookAhead #-}
  lookAheadM (Lazy.RWST m) = Lazy.RWST (\r s -> liftM factor $ lookAheadE $ liftM distribute $ m r s )
    where
    distribute (Nothing, s',w') = Left (Nothing, s', w')
    distribute (Just a, s',w') = Right (Just a, s', w')
    factor = either id id
  {-# INLINE lookAheadM #-}
  lookAheadE (Lazy.RWST m) = Lazy.RWST (\r s -> liftM factor $ lookAheadE $ liftM distribute $ m r s)
    where
    distribute (Left a, s', w') = Left (Left a, s', w')
    distribute (Right b, s', w') = Right (Right b, s', w')
    factor = either id id
  {-# INLINE lookAheadE #-}

instance MonadGet m => MonadGet (ExceptT e m) where
  type Remaining (ExceptT e m) = Remaining m
  type Bytes (ExceptT e m) = Bytes m
  lookAhead = mapExceptT lookAhead
  {-# INLINE lookAhead #-}
  lookAheadM (ExceptT m) = ExceptT (liftM factor $ lookAheadE $ liftM distribute m)
    where
    distribute (Left e) = (Left (Left e))
    distribute (Right j) = (Right (Right j))
    factor = either id id
  {-# INLINE lookAheadM #-}
  lookAheadE (ExceptT m) = ExceptT (liftM factor $ lookAheadE $ liftM distribute m)
    where
    distribute (Left e) = (Left (Left e))
    distribute (Right a) = (Right (Right a))
    factor = either id id
  {-# INLINE lookAheadE #-}

-- | Get something from a lazy 'Lazy.ByteString' using 'B.runGet'.
runGetL :: B.Get a -> Lazy.ByteString -> a
runGetL = B.runGet

-- | Get something from a strict 'Strict.ByteString' using 'S.runGet'.
runGetS :: S.Get a -> Strict.ByteString -> Either String a
runGetS = S.runGet
