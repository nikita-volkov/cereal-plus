{-# LANGUAGE UndecidableInstances #-}
module CerealPlus.Serializable (Serializable(..)) where

import CerealPlus.Prelude
import qualified CerealPlus.SerializeT as SerializeT; import CerealPlus.SerializeT (Serialize, SerializeT)
import qualified CerealPlus.DeserializeT as DeserializeT; import CerealPlus.DeserializeT (Deserialize, DeserializeT)
import qualified Data.Serialize as Cereal
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Time as Time
import qualified Data.Time.Clock.TAI as Time
import qualified Data.Tree
import qualified Data.Vector.Generic
import qualified Data.Vector.Primitive
import qualified Data.Vector.Storable
import qualified Data.Vector.Unboxed
import qualified Data.Array.IArray
import qualified Data.Map
import qualified Data.IntMap
import qualified Data.Set
import qualified Data.IntSet
import qualified Data.Sequence
import qualified Data.HashMap.Strict
import qualified Data.HashMap.Lazy
import qualified Data.HashSet


class Serializable a where
  serialize :: a -> Serialize ()
  deserialize :: Deserialize a


instance (HasResolution a, Fractional (Fixed a)) => Serializable (Fixed a) where
  serialize = serialize . toRational
  deserialize = fromRational <$> deserialize


-- 'text' instances:

instance Serializable Text where
  serialize = serialize . Text.encodeUtf8
  deserialize = Text.decodeUtf8 <$> deserialize

instance Serializable LazyText where
  serialize = serialize . LazyText.encodeUtf8
  deserialize = LazyText.decodeUtf8 <$> deserialize


-- 'time' instances:

instance Serializable Day where
  serialize = serialize . Time.toModifiedJulianDay
  deserialize = Time.ModifiedJulianDay <$> deserialize

instance Serializable DiffTime where
  serialize = serialize . toRational
  deserialize = fromRational <$> deserialize

instance Serializable UniversalTime where
  serialize = serialize . Time.getModJulianDate
  deserialize = Time.ModJulianDate <$> deserialize

instance Serializable UTCTime where
  serialize a = serialize (Time.utctDay a) *> serialize (Time.utctDayTime a)
  deserialize = Time.UTCTime <$> deserialize <*> deserialize

instance Serializable NominalDiffTime where
  serialize = serialize . toRational
  deserialize = fromRational <$> deserialize

instance Serializable TimeOfDay where
  serialize a = serialize (Time.todHour a) *> serialize (Time.todMin a) *> serialize (Time.todSec a)
  deserialize = Time.TimeOfDay <$> deserialize <*> deserialize <*> deserialize

instance Serializable TimeZone where
  serialize a = 
    serialize (Time.timeZoneMinutes a) *>
    serialize (Time.timeZoneSummerOnly a) *>
    serialize (Time.timeZoneName a)
  deserialize = Time.TimeZone <$> deserialize <*> deserialize <*> deserialize

instance Serializable LocalTime where
  serialize a = serialize (Time.localDay a) *> serialize (Time.localTimeOfDay a)
  deserialize = Time.LocalTime <$> deserialize <*> deserialize

instance Serializable ZonedTime where
  serialize a = serialize (Time.zonedTimeToLocalTime a) *> serialize (Time.zonedTimeZone a)
  deserialize = Time.ZonedTime <$> deserialize <*> deserialize

instance Serializable AbsoluteTime where
  serialize a = serialize $ Time.diffAbsoluteTime a Time.taiEpoch
  deserialize = toAbsoluteTime <$> deserialize
    where
      toAbsoluteTime dt = Time.addAbsoluteTime dt Time.taiEpoch


-- 'cereal' primitive instances wrappers:

instance Serializable Bool where serialize = put; deserialize = get
instance Serializable Char where serialize = put; deserialize = get
instance Serializable Double where serialize = put; deserialize = get
instance Serializable Float where serialize = put; deserialize = get
instance Serializable Int where serialize = put; deserialize = get
instance Serializable Int8 where serialize = put; deserialize = get
instance Serializable Int16 where serialize = put; deserialize = get
instance Serializable Int32 where serialize = put; deserialize = get
instance Serializable Int64 where serialize = put; deserialize = get
instance Serializable Integer where serialize = put; deserialize = get
instance Serializable Ordering where serialize = put; deserialize = get
instance Serializable Word where serialize = put; deserialize = get
instance Serializable Word8 where serialize = put; deserialize = get
instance Serializable Word16 where serialize = put; deserialize = get
instance Serializable Word32 where serialize = put; deserialize = get
instance Serializable Word64 where serialize = put; deserialize = get
instance Serializable () where serialize = put; deserialize = get
instance Serializable ByteString where serialize = put; deserialize = get
instance Serializable LazyByteString where serialize = put; deserialize = get
instance Serializable IntSet where serialize = put; deserialize = get

put :: (Cereal.Serialize a) => a -> Serialize ()
put = SerializeT.liftPut . Cereal.put

get :: (Cereal.Serialize a) => Deserialize a
get = DeserializeT.liftGet Cereal.get


-- Monoid wrappers instances:

instance Serializable a => Serializable (Dual a) where
  serialize = serialize . \case Dual a -> a
  deserialize = Dual <$> deserialize
  
instance Serializable All where
  serialize = serialize . \case All a -> a
  deserialize = All <$> deserialize

instance Serializable Any where
  serialize = serialize . \case Any a -> a
  deserialize = Any <$> deserialize

instance Serializable a => Serializable (Sum a) where
  serialize = serialize . \case Sum a -> a
  deserialize = Sum <$> deserialize

instance Serializable a => Serializable (Product a) where
  serialize = serialize . \case Product a -> a
  deserialize = Product <$> deserialize

instance Serializable a => Serializable (First a) where
  serialize = serialize . \case First a -> a
  deserialize = First <$> deserialize

instance Serializable a => Serializable (Last a) where
  serialize = serialize . \case Last a -> a
  deserialize = Last <$> deserialize


-- Composite instances:

instance (Serializable a, Integral a) => Serializable (Ratio a) where
  serialize a = serialize (numerator a) *> serialize (denominator a)
  deserialize = (%) <$> deserialize <*> deserialize

instance Serializable e => Serializable (Tree e) where
  serialize (Data.Tree.Node root sub) = serialize root *> serialize sub
  deserialize = Data.Tree.Node <$> deserialize <*> deserialize

instance (Serializable a, Serializable b) => Serializable (Either a b) where
  serialize = \case
    Right a -> serialize True *> serialize a
    Left a -> serialize False *> serialize a
  deserialize = do
    deserialize >>= \case
      True -> Right <$> deserialize
      False -> Left <$> deserialize

instance Serializable a => Serializable (Maybe a) where
  serialize = \case 
    Just a -> serialize True >> serialize a
    Nothing -> serialize False
  deserialize = do
    z <- deserialize
    if z
      then deserialize >>= return . Just
      else return Nothing

instance Serializable a => Serializable [a] where
  serialize l = do
    serialize (length l)
    mapM_ serialize l
  deserialize = do
    n <- deserialize
    replicateM n deserialize

instance Serializable e => Serializable (Seq e) where
  serialize = serialize . toList
  deserialize = Data.Sequence.fromList <$> deserialize


-- Tuple instances:

instance Serializable a => Serializable (Identity a) where
  serialize = serialize . \case Identity a -> a
  deserialize = Identity <$> deserialize

instance (Serializable a, Serializable b) => Serializable (a, b) where
  serialize (a, b) = serialize a *> serialize b
  deserialize = (,) <$> deserialize <*> deserialize
  
instance (Serializable a, Serializable b, Serializable c) => Serializable (a, b, c) where
  serialize (a, b, c) = serialize a *> serialize b *> serialize c
  deserialize = (,,) <$> deserialize <*> deserialize <*> deserialize
  
instance (Serializable a, Serializable b, Serializable c, Serializable d) => Serializable (a, b, c, d) where
  serialize (a, b, c, d) = serialize a *> serialize b *> serialize c *> serialize d
  deserialize = (,,,) <$> deserialize <*> deserialize <*> deserialize <*> deserialize

instance (Serializable a, Serializable b, Serializable c, Serializable d, Serializable e) => Serializable (a, b, c, d, e) where
  serialize (a, b, c, d, e) = serialize a *> serialize b *> serialize c *> serialize d *> serialize e
  deserialize = (,,,,) <$> deserialize <*> deserialize <*> deserialize <*> deserialize <*> deserialize


-- 'containers' instances:

instance (Ord a, Serializable a) => Serializable (Set a) where
  serialize = serialize . Data.Set.toAscList
  deserialize = Data.Set.fromDistinctAscList <$> deserialize

instance Serializable e => Serializable (IntMap e) where
  serialize = serialize . Data.IntMap.toAscList
  deserialize = Data.IntMap.fromDistinctAscList <$> deserialize

instance (Ord k, Serializable k, Serializable v) => Serializable (Map k v) where
  serialize = serialize . Data.Map.toAscList
  deserialize = Data.Map.fromDistinctAscList <$> deserialize


-- 'unordered-containers' instances:

instance (Hashable a, Eq a, Serializable a, Serializable b) => Serializable (HashMap a b) where
  serialize = serialize . Data.HashMap.Lazy.toList
  deserialize = Data.HashMap.Lazy.fromList <$> deserialize

instance (Hashable a, Eq a, Serializable a) => Serializable (HashSet a) where
  serialize = serialize . Data.HashSet.toList
  deserialize = Data.HashSet.fromList <$> deserialize


-- 'array' instances:

instance (Ix i, Serializable e, Serializable i) => Serializable (Array i e) where
  serialize = serializeArray
  deserialize = deserializeArray

instance (IArray UArray e, Ix i, Serializable e, Serializable i) => Serializable (UArray i e) where
  serialize = serializeArray
  deserialize = deserializeArray

serializeArray :: (Ix i, Serializable e, Serializable i, IArray a e) => a i e -> Serialize ()
serializeArray a = do 
  serialize $ Data.Array.IArray.bounds a
  serialize $ Data.Array.IArray.elems a

deserializeArray :: (Ix i, Serializable e, Serializable i, IArray a e) => Deserialize (a i e)
deserializeArray = Data.Array.IArray.listArray <$> deserialize <*> deserialize


-- 'vector' instances:

instance Serializable a => Serializable (Vector a) where
  serialize = serializeVector
  deserialize = deserializeVector

instance (Serializable a, Data.Vector.Primitive.Prim a) => Serializable (PVector a) where
  serialize = serializeVector
  deserialize = deserializeVector

instance (Serializable a, Data.Vector.Storable.Storable a) => Serializable (SVector a) where
  serialize = serializeVector
  deserialize = deserializeVector

instance (Serializable a, Data.Vector.Unboxed.Unbox a) => Serializable (UVector a) where
  serialize = serializeVector
  deserialize = deserializeVector

serializeVector :: (Data.Vector.Generic.Vector v a, Serializable a) => v a -> Serialize ()
serializeVector a = do
  serialize (Data.Vector.Generic.length a)
  Data.Vector.Generic.mapM_ serialize a

deserializeVector :: (Data.Vector.Generic.Vector v a, Serializable a) => Deserialize (v a)
deserializeVector = do
  length <- deserialize
  Data.Vector.Generic.replicateM length deserialize
