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


class Serializable a m where
  serialize :: a -> SerializeT m ()
  deserialize :: DeserializeT m a


instance (Applicative m, Monad m, HasResolution a, Fractional (Fixed a)) => Serializable (Fixed a) m where
  serialize = serialize . toRational
  deserialize = fromRational <$> deserialize


-- 'text' instances:

instance (Applicative m, Monad m) => Serializable Text m where
  serialize = serialize . Text.encodeUtf8
  deserialize = Text.decodeUtf8 <$> deserialize

instance (Applicative m, Monad m) => Serializable LazyText m where
  serialize = serialize . LazyText.encodeUtf8
  deserialize = LazyText.decodeUtf8 <$> deserialize


-- 'time' instances:

instance (Applicative m, Monad m) => Serializable Day m where
  serialize = serialize . Time.toModifiedJulianDay
  deserialize = Time.ModifiedJulianDay <$> deserialize

instance (Applicative m, Monad m) => Serializable DiffTime m where
  serialize = serialize . toRational
  deserialize = fromRational <$> deserialize

instance (Applicative m, Monad m) => Serializable UniversalTime m where
  serialize = serialize . Time.getModJulianDate
  deserialize = Time.ModJulianDate <$> deserialize

instance (Applicative m, Monad m) => Serializable UTCTime m where
  serialize a = serialize (Time.utctDay a) *> serialize (Time.utctDayTime a)
  deserialize = Time.UTCTime <$> deserialize <*> deserialize

instance (Applicative m, Monad m) => Serializable NominalDiffTime m where
  serialize = serialize . toRational
  deserialize = fromRational <$> deserialize

instance (Applicative m, Monad m) => Serializable TimeOfDay m where
  serialize a = serialize (Time.todHour a) *> serialize (Time.todMin a) *> serialize (Time.todSec a)
  deserialize = Time.TimeOfDay <$> deserialize <*> deserialize <*> deserialize

instance (Applicative m, Monad m) => Serializable TimeZone m where
  serialize a = 
    serialize (Time.timeZoneMinutes a) *>
    serialize (Time.timeZoneSummerOnly a) *>
    serialize (Time.timeZoneName a)
  deserialize = Time.TimeZone <$> deserialize <*> deserialize <*> deserialize

instance (Applicative m, Monad m) => Serializable LocalTime m where
  serialize a = serialize (Time.localDay a) *> serialize (Time.localTimeOfDay a)
  deserialize = Time.LocalTime <$> deserialize <*> deserialize

instance (Applicative m, Monad m) => Serializable ZonedTime m where
  serialize a = serialize (Time.zonedTimeToLocalTime a) *> serialize (Time.zonedTimeZone a)
  deserialize = Time.ZonedTime <$> deserialize <*> deserialize

instance (Applicative m, Monad m) => Serializable AbsoluteTime m where
  serialize a = serialize $ Time.diffAbsoluteTime a Time.taiEpoch
  deserialize = toAbsoluteTime <$> deserialize
    where
      toAbsoluteTime dt = Time.addAbsoluteTime dt Time.taiEpoch


-- 'cereal' primitive instances wrappers:

instance (Applicative m, Monad m) => Serializable Bool m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable Char m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable Double m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable Float m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable Int m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable Int8 m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable Int16 m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable Int32 m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable Int64 m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable Integer m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable Ordering m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable Word m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable Word8 m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable Word16 m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable Word32 m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable Word64 m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable () m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable ByteString m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable LazyByteString m where serialize = put; deserialize = get
instance (Applicative m, Monad m) => Serializable IntSet m where serialize = put; deserialize = get

put :: (Applicative m, Monad m, Cereal.Serialize a) => a -> SerializeT m ()
put = SerializeT.liftPut . Cereal.put

get :: (Applicative m, Monad m, Cereal.Serialize a) => DeserializeT m a
get = DeserializeT.liftGet Cereal.get


-- Monoid wrappers instances:

instance (Applicative m, Monad m, Serializable a m) => Serializable (Dual a) m where
  serialize = serialize . \case Dual a -> a
  deserialize = Dual <$> deserialize
  
instance (Applicative m, Monad m) => Serializable All m where
  serialize = serialize . \case All a -> a
  deserialize = All <$> deserialize

instance (Applicative m, Monad m) => Serializable Any m where
  serialize = serialize . \case Any a -> a
  deserialize = Any <$> deserialize

instance (Applicative m, Monad m, Serializable a m) => Serializable (Sum a) m where
  serialize = serialize . \case Sum a -> a
  deserialize = Sum <$> deserialize

instance (Applicative m, Monad m, Serializable a m) => Serializable (Product a) m where
  serialize = serialize . \case Product a -> a
  deserialize = Product <$> deserialize

instance (Applicative m, Monad m, Serializable a m) => Serializable (First a) m where
  serialize = serialize . \case First a -> a
  deserialize = First <$> deserialize

instance (Applicative m, Monad m, Serializable a m) => Serializable (Last a) m where
  serialize = serialize . \case Last a -> a
  deserialize = Last <$> deserialize


-- Composite instances:

instance (Applicative m, Monad m, Serializable a m, Integral a) => Serializable (Ratio a) m where
  serialize a = serialize (numerator a) *> serialize (denominator a)
  deserialize = (%) <$> deserialize <*> deserialize

instance (Applicative m, Monad m, Serializable a m) => Serializable (Tree a) m where
  serialize (Data.Tree.Node root sub) = serialize root *> serialize sub
  deserialize = Data.Tree.Node <$> deserialize <*> deserialize

instance (Applicative m, Monad m, Serializable a m, Serializable b m) => Serializable (Either a b) m where
  serialize = \case
    Right a -> serialize True *> serialize a
    Left a -> serialize False *> serialize a
  deserialize = do
    deserialize >>= \case
      True -> Right <$> deserialize
      False -> Left <$> deserialize

instance (Applicative m, Monad m, Serializable a m) => Serializable (Maybe a) m where
  serialize = \case 
    Just a -> serialize True >> serialize a
    Nothing -> serialize False
  deserialize = do
    z <- deserialize
    if z
      then deserialize >>= return . Just
      else return Nothing

instance (Applicative m, Monad m, Serializable a m) => Serializable [a] m where
  serialize l = do
    serialize (length l)
    mapM_ serialize l
  deserialize = do
    n <- deserialize
    replicateM n deserialize

instance (Applicative m, Monad m, Serializable a m) => Serializable (Seq a) m where
  serialize = serialize . toList
  deserialize = Data.Sequence.fromList <$> deserialize


-- Tuple instances:

instance (Applicative m, Monad m, Serializable a m) => Serializable (Identity a) m where
  serialize = serialize . \case Identity a -> a
  deserialize = Identity <$> deserialize

instance (Applicative m, Monad m, Serializable a m, Serializable b m) => Serializable (a, b) m where
  serialize (a, b) = serialize a *> serialize b
  deserialize = (,) <$> deserialize <*> deserialize
  
instance (Applicative m, Monad m, Serializable a m, Serializable b m, Serializable c m) => Serializable (a, b, c) m where
  serialize (a, b, c) = serialize a *> serialize b *> serialize c
  deserialize = (,,) <$> deserialize <*> deserialize <*> deserialize
  
instance (Applicative m, Monad m, Serializable a m, Serializable b m, Serializable c m, Serializable d m) => Serializable (a, b, c, d) m where
  serialize (a, b, c, d) = serialize a *> serialize b *> serialize c *> serialize d
  deserialize = (,,,) <$> deserialize <*> deserialize <*> deserialize <*> deserialize

instance (Applicative m, Monad m, Serializable a m, Serializable b m, Serializable c m, Serializable d m, Serializable e m) => Serializable (a, b, c, d, e) m where
  serialize (a, b, c, d, e) = serialize a *> serialize b *> serialize c *> serialize d *> serialize e
  deserialize = (,,,,) <$> deserialize <*> deserialize <*> deserialize <*> deserialize <*> deserialize


-- 'containers' instances:

instance (Applicative m, Monad m, Serializable a m, Ord a) => Serializable (Set a) m where
  serialize = serialize . Data.Set.toAscList
  deserialize = Data.Set.fromDistinctAscList <$> deserialize

instance (Applicative m, Monad m, Serializable a m) => Serializable (IntMap a) m where
  serialize = serialize . Data.IntMap.toAscList
  deserialize = Data.IntMap.fromDistinctAscList <$> deserialize

instance (Applicative m, Monad m, Serializable a m, Serializable b m, Ord a) => Serializable (Map a b) m where
  serialize = serialize . Data.Map.toAscList
  deserialize = Data.Map.fromDistinctAscList <$> deserialize


-- 'unordered-containers' instances:

instance (Applicative m, Monad m, Serializable a m, Serializable b m, Hashable a, Eq a) => Serializable (HashMap a b) m where
  serialize = serialize . Data.HashMap.Lazy.toList
  deserialize = Data.HashMap.Lazy.fromList <$> deserialize

instance (Applicative m, Monad m, Serializable a m, Hashable a, Eq a) => Serializable (HashSet a) m where
  serialize = serialize . Data.HashSet.toList
  deserialize = Data.HashSet.fromList <$> deserialize


-- 'array' instances:

instance (Applicative m, Monad m, Serializable e m, Serializable i m, Ix i) => Serializable (Array i e) m where
  serialize = serializeArray
  deserialize = deserializeArray

instance (Applicative m, Monad m, Serializable e m, Serializable i m, IArray UArray e, Ix i) => Serializable (UArray i e) m where
  serialize = serializeArray
  deserialize = deserializeArray

serializeArray :: (Applicative m, Monad m, Ix i, Serializable e m, Serializable i m, IArray a e) => a i e -> SerializeT m ()
serializeArray a = do 
  serialize $ Data.Array.IArray.bounds a
  serialize $ Data.Array.IArray.elems a

deserializeArray :: (Applicative m, Monad m, Ix i, Serializable e m, Serializable i m, IArray a e) => DeserializeT m (a i e)
deserializeArray = Data.Array.IArray.listArray <$> deserialize <*> deserialize


-- 'vector' instances:

instance (Applicative m, Monad m, Serializable a m) => Serializable (Vector a) m where
  serialize = serializeVector
  deserialize = deserializeVector

instance (Applicative m, Monad m, Serializable a m, Data.Vector.Primitive.Prim a) => Serializable (PVector a) m where
  serialize = serializeVector
  deserialize = deserializeVector

instance (Applicative m, Monad m, Serializable a m, Data.Vector.Storable.Storable a) => Serializable (SVector a) m where
  serialize = serializeVector
  deserialize = deserializeVector

instance (Applicative m, Monad m, Serializable a m, Data.Vector.Unboxed.Unbox a) => Serializable (UVector a) m where
  serialize = serializeVector
  deserialize = deserializeVector

serializeVector :: (Applicative m, Monad m, Data.Vector.Generic.Vector v a, Serializable a m) => v a -> SerializeT m ()
serializeVector a = do
  serialize (Data.Vector.Generic.length a)
  Data.Vector.Generic.mapM_ serialize a

deserializeVector :: (Applicative m, Monad m, Data.Vector.Generic.Vector v a, Serializable a m) => DeserializeT m (v a)
deserializeVector = do
  length <- deserialize
  Data.Vector.Generic.replicateM length deserialize
