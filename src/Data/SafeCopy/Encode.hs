{-# LANGUAGE ScopedTypeVariables #-}
module Data.SafeCopy.Encode where

import Data.ByteString (ByteString)
import Data.Monoid
import Data.Store
import Data.Store.Core

data Encode a = Encode {-# UNPACK #-} !Int !(Poke ()) !a

instance Monoid a => Monoid (Encode a) where
  mempty = Encode 0 (Poke $ \_ offset -> pure (offset, ())) mempty
  {-# INLINE mempty #-}

  (Encode len1 f1 a1) `mappend` (Encode len2 f2 a2) = Encode (len1 + len2) (f1 *> f2) (a1 <> a2)
  {-# INLINE mappend #-}

instance Functor Encode where
  fmap f (Encode l p a) = Encode l p (f a)
  {-# INLINE fmap #-}

instance Applicative Encode where
  pure = Encode 0 (Poke $ \_ offset -> pure (offset, ()))
  {-# INLINE pure #-}

  (Encode len1 f1 af) <*> (Encode len2 f2 b) = Encode (len1 + len2) (f1 *> f2) (af b)
  {-# INLINE (<*>) #-}

instance Monad Encode where
  return = pure
  {-# INLINE return #-}

  (Encode len1 f1 a) >>= ef = case ef a of
    Encode len2 f2 b -> Encode (len1 + len2) (f1 *> f2) b
  {-# INLINE (>>=) #-}

getEncodeLen :: Encode a -> Int
getEncodeLen (Encode len _ _) = len
{-# INLINE getEncodeLen #-}

runEncode :: Encode a -> ByteString
runEncode (Encode len f _) = unsafeEncodeWith f len
{-# INLINE runEncode #-}

pokeE :: forall a . Store a => a -> Encode a
pokeE v = Encode len (poke v) v
  where
    len = case (size :: Size a) of
      VarSize sf -> sf v
      ConstSize cs -> cs
{-# INLINE pokeE #-}
