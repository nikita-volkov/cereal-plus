module CerealPlus.Serializable (Serializable(..)) where

import CerealPlus.Prelude
import qualified CerealPlus.Serialize as Serialize; import CerealPlus.Serialize (Serialize)
import qualified CerealPlus.Deserialize as Deserialize; import CerealPlus.Deserialize (Deserialize)
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
import qualified Data.HashTable.Class as Hashtables_Class
import qualified Data.HashTable.IO as Hashtables_IO
import qualified Data.HashTable.ST.Basic as Hashtables_Basic
import qualified Data.HashTable.ST.Cuckoo as Hashtables_Cuckoo
import qualified Data.HashTable.ST.Linear as Hashtables_Linear


-- |
-- Support for serialization of a data type in a monadic context 
-- (e.g., 'IO', 'ST', 'Control.Concurrent.STM.STM', 'Identity'),
-- meaning that this can be used to provide serialization support for mutable data.
-- 
-- To use it in a pure context, use 'Identity' monad.
class Serializable a m where
  serialize :: (Monad m, Applicative m) => a -> Serialize m ()
  deserialize :: (Monad m, Applicative m) => Deserialize m a


instance (HasResolution a, Fractional (Fixed a)) => Serializable (Fixed a) m where
  serialize = serialize . toRational
  deserialize = fromRational <$> deserialize


-- 'text' instances:

instance Serializable Text m where
  serialize = serialize . Text.encodeUtf8
  deserialize = Text.decodeUtf8 <$> deserialize

instance Serializable LazyText m where
  serialize = serialize . LazyText.encodeUtf8
  deserialize = LazyText.decodeUtf8 <$> deserialize


-- 'time' instances:

instance Serializable Day m where
  serialize = serialize . Time.toModifiedJulianDay
  deserialize = Time.ModifiedJulianDay <$> deserialize

instance Serializable DiffTime m where
  serialize = serialize . toRational
  deserialize = fromRational <$> deserialize

instance Serializable UniversalTime m where
  serialize = serialize . Time.getModJulianDate
  deserialize = Time.ModJulianDate <$> deserialize

instance Serializable UTCTime m where
  serialize a = serialize (Time.utctDay a) *> serialize (Time.utctDayTime a)
  deserialize = Time.UTCTime <$> deserialize <*> deserialize

instance Serializable NominalDiffTime m where
  serialize = serialize . toRational
  deserialize = fromRational <$> deserialize

instance Serializable TimeOfDay m where
  serialize a = serialize (Time.todHour a) *> serialize (Time.todMin a) *> serialize (Time.todSec a)
  deserialize = Time.TimeOfDay <$> deserialize <*> deserialize <*> deserialize

instance Serializable TimeZone m where
  serialize a = 
    serialize (Time.timeZoneMinutes a) *>
    serialize (Time.timeZoneSummerOnly a) *>
    serialize (Time.timeZoneName a)
  deserialize = Time.TimeZone <$> deserialize <*> deserialize <*> deserialize

instance Serializable LocalTime m where
  serialize a = serialize (Time.localDay a) *> serialize (Time.localTimeOfDay a)
  deserialize = Time.LocalTime <$> deserialize <*> deserialize

instance Serializable ZonedTime m where
  serialize a = serialize (Time.zonedTimeToLocalTime a) *> serialize (Time.zonedTimeZone a)
  deserialize = Time.ZonedTime <$> deserialize <*> deserialize

instance Serializable AbsoluteTime m where
  serialize a = serialize $ Time.diffAbsoluteTime a Time.taiEpoch
  deserialize = toAbsoluteTime <$> deserialize
    where
      toAbsoluteTime dt = Time.addAbsoluteTime dt Time.taiEpoch


-- 'cereal' primitive instances wrappers:

instance Serializable Bool m where serialize = put; deserialize = get
instance Serializable Char m where serialize = put; deserialize = get
instance Serializable Double m where serialize = put; deserialize = get
instance Serializable Float m where serialize = put; deserialize = get
instance Serializable Int m where serialize = put; deserialize = get
instance Serializable Int8 m where serialize = put; deserialize = get
instance Serializable Int16 m where serialize = put; deserialize = get
instance Serializable Int32 m where serialize = put; deserialize = get
instance Serializable Int64 m where serialize = put; deserialize = get
instance Serializable Integer m where serialize = put; deserialize = get
instance Serializable Ordering m where serialize = put; deserialize = get
instance Serializable Word m where serialize = put; deserialize = get
instance Serializable Word8 m where serialize = put; deserialize = get
instance Serializable Word16 m where serialize = put; deserialize = get
instance Serializable Word32 m where serialize = put; deserialize = get
instance Serializable Word64 m where serialize = put; deserialize = get
instance Serializable () m where serialize = put; deserialize = get
instance Serializable ByteString m where serialize = put; deserialize = get
instance Serializable LazyByteString m where serialize = put; deserialize = get
instance Serializable IntSet m where serialize = put; deserialize = get

put :: (Monad m, Applicative m, Cereal.Serialize a) => a -> Serialize m ()
put = Serialize.liftPut . Cereal.put

get :: (Monad m, Applicative m, Cereal.Serialize a) => Deserialize m a
get = Deserialize.liftGet Cereal.get


-- Monoid wrappers instances:

instance (Serializable a m) => Serializable (Dual a) m where
  serialize = serialize . \case Dual a -> a
  deserialize = Dual <$> deserialize
  
instance Serializable All m where
  serialize = serialize . \case All a -> a
  deserialize = All <$> deserialize

instance Serializable Any m where
  serialize = serialize . \case Any a -> a
  deserialize = Any <$> deserialize

instance (Serializable a m) => Serializable (Sum a) m where
  serialize = serialize . \case Sum a -> a
  deserialize = Sum <$> deserialize

instance (Serializable a m) => Serializable (Product a) m where
  serialize = serialize . \case Product a -> a
  deserialize = Product <$> deserialize

instance (Serializable a m) => Serializable (First a) m where
  serialize = serialize . \case First a -> a
  deserialize = First <$> deserialize

instance (Serializable a m) => Serializable (Last a) m where
  serialize = serialize . \case Last a -> a
  deserialize = Last <$> deserialize


-- Composite instances:

instance (Serializable a m, Integral a) => Serializable (Ratio a) m where
  serialize a = serialize (numerator a) *> serialize (denominator a)
  deserialize = (%) <$> deserialize <*> deserialize

instance (Serializable a m) => Serializable (Tree a) m where
  serialize (Data.Tree.Node root sub) = serialize root *> serialize sub
  deserialize = Data.Tree.Node <$> deserialize <*> deserialize

instance (Serializable a m, Serializable b m) => Serializable (Either a b) m where
  serialize = \case
    Right a -> serialize True *> serialize a
    Left a -> serialize False *> serialize a
  deserialize = do
    deserialize >>= \case
      True -> Right <$> deserialize
      False -> Left <$> deserialize

instance (Serializable a m) => Serializable (Maybe a) m where
  serialize = \case 
    Just a -> serialize True >> serialize a
    Nothing -> serialize False
  deserialize = do
    z <- deserialize
    if z
      then deserialize >>= return . Just
      else return Nothing

instance (Serializable a m) => Serializable [a] m where
  serialize l = do
    serialize (length l)
    mapM_ serialize l
  deserialize = do
    n <- deserialize
    replicateM n deserialize

instance (Serializable a m) => Serializable (Seq a) m where
  serialize = serialize . toList
  deserialize = Data.Sequence.fromList <$> deserialize


-- Tuple instances:

instance (Serializable a m) => Serializable (Identity a) m where
  serialize = serialize . \case Identity a -> a
  deserialize = Identity <$> deserialize

instance (Serializable a m, Serializable b m) => Serializable (a, b) m where
  serialize (a, b) = serialize a *> serialize b
  deserialize = (,) <$> deserialize <*> deserialize
  
instance (Serializable a m, Serializable b m, Serializable c m) => Serializable (a, b, c) m where
  serialize (a, b, c) = serialize a *> serialize b *> serialize c
  deserialize = (,,) <$> deserialize <*> deserialize <*> deserialize
  
instance (Serializable a m, Serializable b m, Serializable c m, Serializable d m) => Serializable (a, b, c, d) m where
  serialize (a, b, c, d) = serialize a *> serialize b *> serialize c *> serialize d
  deserialize = (,,,) <$> deserialize <*> deserialize <*> deserialize <*> deserialize

instance (Serializable a m, Serializable b m, Serializable c m, Serializable d m, Serializable e m) => Serializable (a, b, c, d, e) m where
  serialize (a, b, c, d, e) = serialize a *> serialize b *> serialize c *> serialize d *> serialize e
  deserialize = (,,,,) <$> deserialize <*> deserialize <*> deserialize <*> deserialize <*> deserialize


-- 'containers' instances:

instance (Serializable a m, Ord a) => Serializable (Set a) m where
  serialize = serialize . Data.Set.toAscList
  deserialize = Data.Set.fromDistinctAscList <$> deserialize

instance (Serializable a m) => Serializable (IntMap a) m where
  serialize = serialize . Data.IntMap.toAscList
  deserialize = Data.IntMap.fromDistinctAscList <$> deserialize

instance (Serializable a m, Serializable b m, Ord a) => Serializable (Map a b) m where
  serialize = serialize . Data.Map.toAscList
  deserialize = Data.Map.fromDistinctAscList <$> deserialize


-- 'unordered-containers' instances:

instance (Serializable a m, Serializable b m, Hashable a, Eq a) => Serializable (HashMap a b) m where
  serialize = serialize . Data.HashMap.Lazy.toList
  deserialize = Data.HashMap.Lazy.fromList <$> deserialize

instance (Serializable a m, Hashable a, Eq a) => Serializable (HashSet a) m where
  serialize = serialize . Data.HashSet.toList
  deserialize = Data.HashSet.fromList <$> deserialize


-- 'array' instances:

instance (Serializable e m, Serializable i m, Ix i) => Serializable (Array i e) m where
  serialize = serializeArray
  deserialize = deserializeArray

instance (Serializable e m, Serializable i m, IArray UArray e, Ix i) => Serializable (UArray i e) m where
  serialize = serializeArray
  deserialize = deserializeArray

serializeArray :: (Monad m, Applicative m, Ix i, Serializable e m, Serializable i m, IArray a e) => a i e -> Serialize m ()
serializeArray a = do 
  serialize $ Data.Array.IArray.bounds a
  serialize $ Data.Array.IArray.elems a

deserializeArray :: (Monad m, Applicative m, Ix i, Serializable e m, Serializable i m, IArray a e) => Deserialize m (a i e)
deserializeArray = Data.Array.IArray.listArray <$> deserialize <*> deserialize


-- 'vector' instances:

instance (Serializable a m) => Serializable (Vector a) m where
  serialize = serializeVector
  deserialize = deserializeVector

instance (Serializable a m, Data.Vector.Primitive.Prim a) => Serializable (PVector a) m where
  serialize = serializeVector
  deserialize = deserializeVector

instance (Serializable a m, Data.Vector.Storable.Storable a) => Serializable (SVector a) m where
  serialize = serializeVector
  deserialize = deserializeVector

instance (Serializable a m, Data.Vector.Unboxed.Unbox a) => Serializable (UVector a) m where
  serialize = serializeVector
  deserialize = deserializeVector

serializeVector :: (Monad m, Applicative m, Data.Vector.Generic.Vector v a, Serializable a m) => v a -> Serialize m ()
serializeVector a = do
  serialize (Data.Vector.Generic.length a)
  Data.Vector.Generic.mapM_ serialize a

deserializeVector :: (Monad m, Applicative m, Data.Vector.Generic.Vector v a, Serializable a m) => Deserialize m (v a)
deserializeVector = do
  length <- deserialize
  Data.Vector.Generic.replicateM length deserialize


-- 'hashtables' instances:

instance ( Serializable a (ST s), Serializable b (ST s), Hashable a, Eq a ) => 
         Serializable (Hashtables_Basic.HashTable s a b) (ST s) where
  serialize = serializeHashTableST
  deserialize = deserializeHashTableST

instance ( Serializable a (ST s), Serializable b (ST s), Hashable a, Eq a ) => 
         Serializable (Hashtables_Cuckoo.HashTable s a b) (ST s) where
  serialize = serializeHashTableST
  deserialize = deserializeHashTableST

instance ( Serializable a (ST s), Serializable b (ST s), Hashable a, Eq a ) => 
         Serializable (Hashtables_Linear.HashTable s a b) (ST s) where
  serialize = serializeHashTableST
  deserialize = deserializeHashTableST

serializeHashTableST :: 
  (Hashtables_Class.HashTable t, Serializable a (ST s), Serializable b (ST s)) => 
  t s a b -> Serialize (ST s) ()
serializeHashTableST t = do
  join $ lift $ Hashtables_Class.foldM (\a b -> return $ a >> processRow b) (return ()) t
  signalEnd
  where
    processRow (k, v) = do
      signalRow
      serialize k
      serialize v
    signalRow = serialize True
    signalEnd = serialize False

deserializeHashTableST :: 
  (Hashtables_Class.HashTable t, Serializable a (ST s), Serializable b (ST s), Hashable a, Eq a) => 
  Deserialize (ST s) (t s a b)
deserializeHashTableST = do
  t <- lift $ Hashtables_Class.new
  loop $ do
    (k, v) <- deserializeRow
    lift $ Hashtables_Class.insert t k v
  return t
  where
    loop action = do
      deserialize >>= \case
        False -> return ()
        True -> action >> loop action
    deserializeRow = (,) <$> deserialize <*> deserialize

instance ( Serializable a (ST RealWorld), Serializable b (ST RealWorld), Hashable a, Eq a ) => 
         Serializable (Hashtables_Basic.HashTable RealWorld a b) IO where
  serialize = Serialize.mapBase stToIO . serializeHashTableST
  deserialize = Deserialize.mapBase stToIO deserializeHashTableST

instance ( Serializable a (ST RealWorld), Serializable b (ST RealWorld), Hashable a, Eq a ) => 
         Serializable (Hashtables_Cuckoo.HashTable RealWorld a b) IO where
  serialize = Serialize.mapBase stToIO . serializeHashTableST
  deserialize = Deserialize.mapBase stToIO deserializeHashTableST

instance ( Serializable a (ST RealWorld), Serializable b (ST RealWorld), Hashable a, Eq a ) => 
         Serializable (Hashtables_Linear.HashTable RealWorld a b) IO where
  serialize = Serialize.mapBase stToIO . serializeHashTableST
  deserialize = Deserialize.mapBase stToIO deserializeHashTableST

