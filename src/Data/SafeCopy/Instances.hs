{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, UndecidableInstances, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.SafeCopy.Instances where

import Data.SafeCopy.SafeCopy
import Data.SafeCopy.Encode

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Monad
import qualified Data.Array as Array
import qualified Data.Array.Unboxed as UArray
import qualified Data.Array.IArray as IArray
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as Foldable
import           Data.Fixed (HasResolution, Fixed)
import           Data.Int
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import           Data.Ix
import qualified Data.Map as Map
import           Data.Ratio (Ratio, (%), numerator, denominator)
import qualified Data.Sequence as Sequence
import           Data.Store
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock (DiffTime, NominalDiffTime, UniversalTime(..), UTCTime(..))
import           Data.Time.Clock.TAI (AbsoluteTime, taiEpoch, addAbsoluteTime, diffAbsoluteTime)
import           Data.Time.LocalTime (LocalTime(..), TimeOfDay(..), TimeZone(..), ZonedTime(..))
import qualified Data.Tree as Tree
#if MIN_VERSION_base(4,7,0)
import           Data.Typeable hiding (Proxy)
#else
import           Data.Typeable
#endif
import           Data.Word
import           System.Time (ClockTime(..), TimeDiff(..), CalendarTime(..), Month(..))
import qualified System.Time as OT
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

instance SafeCopy a => SafeCopy (Prim a) where
  kind = primitive
  getCopy = contain $
            do e <- unsafeUnPack getCopy
               return $ Prim e
  putCopy (Prim e)
    = contain $ unsafeUnPack (putCopy e) >> pure (Prim e)

instance SafeCopy a => SafeCopy [a] where
  getCopy = contain $ do
    n <- peek
    g <- getSafeGet
    go g [] n
      where
        go :: Peek a -> [a] -> Int -> Peek [a]
        go _ as 0 = return (reverse as)
        go g as i = do x <- g
                       x `seq` go g (x:as) (i - 1)
  putCopy lst = contain $ do void $ pokeE (length lst)
                             getSafePut >>= forM lst
  errorTypeName = typeName1

instance SafeCopy a => SafeCopy (Maybe a) where
    getCopy = contain $ do n <- peek
                           if n then fmap Just safeGet
                                else return Nothing
    putCopy (Just a) = contain $ pokeE True >> safePut a >> pure (Just a)
    putCopy Nothing = contain $ pokeE False >> pure Nothing
    errorTypeName = typeName1

instance (SafeCopy a, Ord a) => SafeCopy (Set.Set a) where
    getCopy = contain $ fmap Set.fromDistinctAscList safeGet
    putCopy a = contain $ safePut (Set.toAscList a) >> pure a
    errorTypeName = typeName1

instance (SafeCopy a, SafeCopy b, Ord a) => SafeCopy (Map.Map a b) where
    getCopy = contain $ fmap Map.fromDistinctAscList safeGet
    putCopy a = contain $ safePut (Map.toAscList a) >> pure a
    errorTypeName = typeName2

instance (SafeCopy a) => SafeCopy (IntMap.IntMap a) where
    getCopy = contain $ fmap IntMap.fromDistinctAscList safeGet
    putCopy a = contain $ safePut (IntMap.toAscList a) >> pure a
    errorTypeName = typeName1

instance SafeCopy IntSet.IntSet where
    getCopy = contain $ fmap IntSet.fromDistinctAscList safeGet
    putCopy a = contain $ safePut (IntSet.toAscList a) >> pure a
    errorTypeName = typeName

instance (SafeCopy a) => SafeCopy (Sequence.Seq a) where
    getCopy = contain $ fmap Sequence.fromList safeGet
    putCopy a = contain $ safePut (Foldable.toList a) >> pure a
    errorTypeName = typeName1

instance (SafeCopy a) => SafeCopy (Tree.Tree a) where
    getCopy = contain $ liftM2 Tree.Node safeGet safeGet
    putCopy a@(Tree.Node root sub) = contain $ safePut root >> safePut sub >> pure a
    errorTypeName = typeName1

iarray_getCopy :: (Ix i, SafeCopy e, SafeCopy i, IArray.IArray a e) => Contained (Peek (a i e))
iarray_getCopy = contain $ do getIx <- getSafeGet
                              liftM3 mkArray getIx getIx safeGet
    where
      mkArray l h = IArray.listArray (l, h)
{-# INLINE iarray_getCopy #-}

iarray_putCopy :: (Ix i, SafeCopy e, SafeCopy i, IArray.IArray a e) => a i e -> Contained (Encode (a i e))
iarray_putCopy arr = contain $ do putIx <- getSafePut
                                  let (l,h) = IArray.bounds arr
                                  _ <- putIx l >> putIx h
                                  _ <- safePut (IArray.elems arr)
                                  pure arr
{-# INLINE iarray_putCopy #-}

instance (Ix i, SafeCopy e, SafeCopy i) => SafeCopy (Array.Array i e) where
    getCopy = iarray_getCopy
    putCopy = iarray_putCopy
    errorTypeName = typeName2

instance (IArray.IArray UArray.UArray e, Ix i, SafeCopy e, SafeCopy i) => SafeCopy (UArray.UArray i e) where
    getCopy = iarray_getCopy
    putCopy = iarray_putCopy
    errorTypeName = typeName2

instance (SafeCopy a, SafeCopy b) => SafeCopy (a,b) where
    getCopy = contain $ liftM2 (,) safeGet safeGet
    putCopy (a,b) = contain $ safePut a >> safePut b >> pure (a,b)
    errorTypeName = typeName2
instance (SafeCopy a, SafeCopy b, SafeCopy c) => SafeCopy (a,b,c) where
    getCopy = contain $ liftM3 (,,) safeGet safeGet safeGet
    putCopy (a,b,c) = contain $ safePut a >> safePut b >> safePut c >> pure (a,b,c)
instance (SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d) => SafeCopy (a,b,c,d) where
    getCopy = contain $ liftM4 (,,,) safeGet safeGet safeGet safeGet
    putCopy (a,b,c,d) = contain $ safePut a >> safePut b >> safePut c >> safePut d >> pure (a,b,c,d)
instance (SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d, SafeCopy e) =>
         SafeCopy (a,b,c,d,e) where
    getCopy = contain $ liftM5 (,,,,) safeGet safeGet safeGet safeGet safeGet
    putCopy (a,b,c,d,e) = contain $ safePut a >> safePut b >> safePut c >> safePut d >> safePut e >> pure (a,b,c,d,e)
instance (SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d, SafeCopy e, SafeCopy f) =>
         SafeCopy (a,b,c,d,e,f) where
    getCopy = contain $ (,,,,,) <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet
    putCopy (a,b,c,d,e,f) = contain $ safePut a >> safePut b >> safePut c >> safePut d >>
                                      safePut e >> safePut f >> pure (a,b,c,d,e,f)
instance (SafeCopy a, SafeCopy b, SafeCopy c, SafeCopy d, SafeCopy e, SafeCopy f, SafeCopy g) =>
         SafeCopy (a,b,c,d,e,f,g) where
    getCopy = contain $ (,,,,,,) <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*>
                                     safeGet <*> safeGet <*> safeGet
    putCopy (a,b,c,d,e,f,g) = contain $ safePut a >> safePut b >> safePut c >> safePut d >>
                                        safePut e >> safePut f >> safePut g >> pure (a,b,c,d,e,f,g)


instance SafeCopy Int where
    getCopy = contain peek; putCopy = contain . pokeE; errorTypeName = typeName
instance SafeCopy Integer where
    getCopy = contain peek; putCopy = contain . pokeE; errorTypeName = typeName

-- | cereal change the formats for Float/Double in 0.5.*
--
-- https://github.com/GaloisInc/cereal/commit/47d839609413e3e9d1147b99c34ae421ae36bced
-- https://github.com/GaloisInc/cereal/issues/35
newtype CerealFloat040 = CerealFloat040 { unCerealFloat040 :: Float} deriving (Show, Typeable)
instance SafeCopy CerealFloat040 where
    getCopy = contain (CerealFloat040 <$> liftM2 encodeFloat peek peek)
    putCopy a@(CerealFloat040 float) = contain $ pokeE (decodeFloat float) >> pure a
    errorTypeName = typeName

instance Migrate Float where
  type MigrateFrom Float = CerealFloat040
  migrate (CerealFloat040 d) = d

instance SafeCopy Float where
  version = Version 1
  kind = extension
  getCopy = contain peek
  putCopy = contain . pokeE
  errorTypeName = typeName

-- | cereal change the formats for Float/Double in 0.5.*
--
-- https://github.com/GaloisInc/cereal/commit/47d839609413e3e9d1147b99c34ae421ae36bced
-- https://github.com/GaloisInc/cereal/issues/35
newtype CerealDouble040 = CerealDouble040 { unCerealDouble040 :: Double} deriving (Show, Typeable)
instance SafeCopy CerealDouble040 where
    getCopy = contain (CerealDouble040 <$> liftM2 encodeFloat peek peek)
    putCopy a@(CerealDouble040 double) = contain $ pokeE (decodeFloat double) >> pure a
    errorTypeName = typeName

instance Migrate Double where
  type MigrateFrom Double = CerealDouble040
  migrate (CerealDouble040 d) = d

instance SafeCopy Double where
  version = Version 1
  kind = extension
  getCopy = contain peek
  putCopy = contain . pokeE
  errorTypeName = typeName

instance Store Ordering

instance SafeCopy L.ByteString where
    getCopy = contain peek; putCopy = contain . pokeE; errorTypeName = typeName
instance SafeCopy B.ByteString where
    getCopy = contain peek; putCopy = contain . pokeE; errorTypeName = typeName
instance SafeCopy Char where
    getCopy = contain peek; putCopy = contain . pokeE; errorTypeName = typeName
instance SafeCopy Word where
    getCopy = contain peek; putCopy = contain . pokeE; errorTypeName = typeName
instance SafeCopy Word8 where
    getCopy = contain peek; putCopy = contain . pokeE; errorTypeName = typeName
instance SafeCopy Word16 where
    getCopy = contain peek; putCopy = contain . pokeE; errorTypeName = typeName
instance SafeCopy Word32 where
    getCopy = contain peek; putCopy = contain . pokeE; errorTypeName = typeName
instance SafeCopy Word64 where
    getCopy = contain peek; putCopy = contain . pokeE; errorTypeName = typeName
instance SafeCopy Ordering where
    getCopy = contain peek; putCopy = contain . pokeE; errorTypeName = typeName
instance SafeCopy Int8 where
    getCopy = contain peek; putCopy = contain . pokeE; errorTypeName = typeName
instance SafeCopy Int16 where
    getCopy = contain peek; putCopy = contain . pokeE; errorTypeName = typeName
instance SafeCopy Int32 where
    getCopy = contain peek; putCopy = contain . pokeE; errorTypeName = typeName
instance SafeCopy Int64 where
    getCopy = contain peek; putCopy = contain . pokeE; errorTypeName = typeName
instance (Integral a, SafeCopy a) => SafeCopy (Ratio a) where
    getCopy   = contain $ do n <- safeGet
                             d <- safeGet
                             return (n % d)
    putCopy r = contain $ do void $ safePut (numerator   r)
                             void $ safePut (denominator r)
                             pure r
    errorTypeName = typeName1
instance (HasResolution a, Fractional (Fixed a)) => SafeCopy (Fixed a) where
    getCopy   = contain $ fromRational <$> safeGet
    putCopy a = contain $ safePut (toRational a) >> pure a
    errorTypeName = typeName1

instance SafeCopy () where
    getCopy = contain peek; putCopy = contain . pokeE; errorTypeName = typeName
instance SafeCopy Bool where
    getCopy = contain peek; putCopy = contain . pokeE; errorTypeName = typeName
instance (SafeCopy a, SafeCopy b) => SafeCopy (Either a b) where
    getCopy = contain $ do n <- peek
                           if n then fmap Right safeGet
                                else fmap Left safeGet
    putCopy e@(Right a) = contain $ pokeE True >> safePut a >> pure e
    putCopy e@(Left a) = contain $ pokeE False >> safePut a >> pure e

    errorTypeName = typeName2

--  instances for 'text' library

instance SafeCopy T.Text where
    kind = base
    getCopy = contain $ T.decodeUtf8 <$> safeGet
    putCopy e = contain $ safePut (T.encodeUtf8 e) >> pure e
    errorTypeName = typeName

instance SafeCopy TL.Text where
    kind = base
    getCopy = contain $ TL.decodeUtf8 <$> safeGet
    putCopy e = contain $ safePut (TL.encodeUtf8 e) >> pure e
    errorTypeName = typeName

-- instances for 'time' library

instance SafeCopy Day where
    kind = base
    getCopy = contain $ ModifiedJulianDay <$> safeGet
    putCopy e = contain $ safePut (toModifiedJulianDay e) >> pure e
    errorTypeName = typeName

instance SafeCopy DiffTime where
    kind = base
    getCopy = contain $ fromRational <$> safeGet
    putCopy e = contain $ safePut (toRational e) >> pure e
    errorTypeName = typeName

instance SafeCopy UniversalTime where
    kind = base
    getCopy = contain $ ModJulianDate <$> safeGet
    putCopy e = contain $ safePut (getModJulianDate e) >> pure e
    errorTypeName = typeName

instance SafeCopy UTCTime where
    kind = base
    getCopy   = contain $ do day      <- safeGet
                             diffTime <- safeGet
                             return (UTCTime day diffTime)
    putCopy u = contain $ do void $ safePut (utctDay u)
                             void $ safePut (utctDayTime u)
                             pure u
    errorTypeName = typeName

instance SafeCopy NominalDiffTime where
    kind = base
    getCopy = contain $ fromRational <$> safeGet
    putCopy e = contain $ safePut (toRational e) >> pure e
    errorTypeName = typeName

instance SafeCopy TimeOfDay where
    kind = base
    getCopy   = contain $ do hour <- safeGet
                             mins <- safeGet
                             sec  <- safeGet
                             return (TimeOfDay hour mins sec)
    putCopy t = contain $ do void $ safePut (todHour t)
                             void $ safePut (todMin t)
                             void $ safePut (todSec t)
                             pure t
    errorTypeName = typeName

instance SafeCopy TimeZone where
    kind = base
    getCopy   = contain $ do mins       <- safeGet
                             summerOnly <- safeGet
                             zoneName   <- safeGet
                             return (TimeZone mins summerOnly zoneName)
    putCopy t = contain $ do void $ safePut (timeZoneMinutes t)
                             void $ safePut (timeZoneSummerOnly t)
                             void $ safePut (timeZoneName t)
                             pure t
    errorTypeName = typeName

instance SafeCopy LocalTime where
    kind = base
    getCopy   = contain $ do day <- safeGet
                             tod <- safeGet
                             return (LocalTime day tod)
    putCopy t = contain $ do void $ safePut (localDay t)
                             void $ safePut (localTimeOfDay t)
                             pure t
    errorTypeName = typeName

instance SafeCopy ZonedTime where
    kind = base
    getCopy   = contain $ do localTime <- safeGet
                             timeZone  <- safeGet
                             return (ZonedTime localTime timeZone)
    putCopy t = contain $ do void $ safePut (zonedTimeToLocalTime t)
                             void $ safePut (zonedTimeZone t)
                             pure t
    errorTypeName = typeName

instance SafeCopy AbsoluteTime where
  getCopy = contain $ fmap toAbsoluteTime safeGet
    where
      toAbsoluteTime :: DiffTime -> AbsoluteTime
      toAbsoluteTime dt = addAbsoluteTime dt taiEpoch
  putCopy e = contain $ safePut (fromAbsoluteTime e) >> pure e
    where
      fromAbsoluteTime :: AbsoluteTime -> DiffTime
      fromAbsoluteTime at = diffAbsoluteTime at taiEpoch
  errorTypeName = typeName

-- instances for old-time

instance SafeCopy ClockTime where
    kind = base
    getCopy = contain $ do secs <- safeGet
                           pico <- safeGet
                           return (TOD secs pico)
    putCopy e@(TOD secs pico) =
              contain $ do void $ safePut secs
                           void $ safePut pico
                           pure e

instance SafeCopy TimeDiff where
    kind = base
    getCopy   = contain $ do year    <- peek
                             month   <- peek
                             day     <- peek
                             hour    <- peek
                             mins    <- peek
                             sec     <- peek
                             pico    <- peek
                             return (TimeDiff year month day hour mins sec pico)
    putCopy t = contain $ do void $ pokeE (tdYear t)
                             void $ pokeE (tdMonth t)
                             void $ pokeE (tdDay t)
                             void $ pokeE (tdHour t)
                             void $ pokeE (tdMin t)
                             void $ pokeE (tdSec t)
                             void $ pokeE (tdPicosec t)
                             pure t

instance SafeCopy OT.Day where
    kind = base ; getCopy = contain $ toEnum <$> peek ; putCopy a = contain $ pokeE (fromEnum a) >> pure a

instance SafeCopy Month where
    kind = base ; getCopy = contain $ toEnum <$> peek ; putCopy a = contain $ pokeE (fromEnum a) >> pure a


instance SafeCopy CalendarTime where
    kind = base
    getCopy   = contain $ do year   <- peek
                             month  <- safeGet
                             day    <- peek
                             hour   <- peek
                             mins   <- peek
                             sec    <- peek
                             pico   <- peek
                             wday   <- safeGet
                             yday   <- peek
                             tzname <- safeGet
                             tz     <- peek
                             dst    <- peek
                             return (CalendarTime year month day hour mins sec pico wday yday tzname tz dst)
    putCopy t = contain $ do void $ pokeE   (ctYear t)
                             void $ safePut (ctMonth t)
                             void $ pokeE   (ctDay t)
                             void $ pokeE   (ctHour t)
                             void $ pokeE   (ctMin t)
                             void $ pokeE   (ctSec t)
                             void $ pokeE   (ctPicosec t)
                             void $ safePut (ctWDay t)
                             void $ pokeE   (ctYDay t)
                             void $ safePut (ctTZName t)
                             void $ pokeE   (ctTZ t)
                             void $ pokeE   (ctIsDST t)
                             pure t

typeName :: Typeable a => Proxy a -> String
typeName proxy = show (typeOf (undefined `asProxyType` proxy))

typeName1 :: (Typeable c) => Proxy (c a) -> String
typeName1 proxy = show (typeOf1 (undefined `asProxyType` proxy))

typeName2 :: (Typeable c) => Proxy (c a b) -> String
typeName2 proxy = show (typeOf2 (undefined `asProxyType` proxy))

getGenericVector :: (SafeCopy a, VG.Vector v a) => Contained (Peek (v a))
getGenericVector = contain $ do n <- peek
                                getSafeGet >>= VG.replicateM n

putGenericVector :: (SafeCopy a, VG.Vector v a) => v a -> Contained (Encode (v a))
putGenericVector v = contain $ do void $ pokeE (VG.length v)
                                  getSafePut >>= VG.forM v

instance SafeCopy a => SafeCopy (V.Vector a) where
    getCopy = getGenericVector
    putCopy = putGenericVector

instance (SafeCopy a, VP.Prim a) => SafeCopy (VP.Vector a) where
    getCopy = getGenericVector
    putCopy = putGenericVector

instance (SafeCopy a, VS.Storable a) => SafeCopy (VS.Vector a) where
    getCopy = getGenericVector
    putCopy = putGenericVector

instance (SafeCopy a, VU.Unbox a) => SafeCopy (VU.Vector a) where
    getCopy = getGenericVector
    putCopy = putGenericVector
