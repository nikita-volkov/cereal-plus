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
import qualified GHC.Generics as Generics; import GHC.Generics ((:+:)(..), (:*:)(..))


-- |
-- Support for serialization of a data type in a monadic context 
-- (e.g., 'IO', 'ST', 'Control.Concurrent.STM.STM', 'Identity'),
-- meaning that this can be used to provide serialization support for mutable data.
-- 
-- To use it in a pure context, refer to 'Identity' monad.
class Serializable m a where
  serialize :: (Monad m, Applicative m) => a -> Serialize m ()
  deserialize :: (Monad m, Applicative m) => Deserialize m a

  default serialize :: 
    (Applicative m, Monad m, Generic a, SerializableRep (Generics.Rep a) m) => 
    a -> Serialize m ()
  serialize = serializeRep . Generics.from

  default deserialize ::
    (Applicative m, Monad m, Generic a, SerializableRep (Generics.Rep a) m) => 
    Deserialize m a
  deserialize = Generics.to <$> deserializeRep


-- Generics:

class SerializableRep r m where
  serializeRep :: (Applicative m, Monad m) => r a -> Serialize m ()
  deserializeRep :: (Applicative m, Monad m) => Deserialize m (r a)

instance SerializableRep Generics.U1 m where
  serializeRep _ = pure ()
  deserializeRep = pure Generics.U1

instance (SerializableRep a m) => SerializableRep (Generics.M1 i c a) m where
  serializeRep = serializeRep . Generics.unM1
  deserializeRep = Generics.M1 <$> deserializeRep

instance (Serializable m a) => SerializableRep (Generics.K1 i a) m where
  serializeRep = serialize . Generics.unK1
  deserializeRep = Generics.K1 <$> deserialize

instance (SerializableRep a m, SerializableRep b m) => SerializableRep (a :*: b) m where
  serializeRep (a :*: b) = serializeRep a *> serializeRep b
  deserializeRep = (:*:) <$> deserializeRep <*> deserializeRep

instance (SerializableRep a m, SerializableRep b m) => SerializableRep (a :+: b) m where
  serializeRep = \case
    Generics.L1 a -> serialize False *> serializeRep a
    Generics.R1 a -> serialize True *> serializeRep a
  deserializeRep = deserialize >>= \case
    False -> Generics.L1 <$> deserializeRep
    True -> Generics.R1 <$> deserializeRep


-- Manual instances:

instance (HasResolution a, Fractional (Fixed a)) => Serializable m (Fixed a) where
  serialize = serialize . toRational
  deserialize = fromRational <$> deserialize


-- 'text' instances:

instance Serializable m Text where
  serialize = serialize . Text.encodeUtf8
  deserialize = Text.decodeUtf8 <$> deserialize

instance Serializable m LazyText where
  serialize = serialize . LazyText.encodeUtf8
  deserialize = LazyText.decodeUtf8 <$> deserialize


-- 'time' instances:

instance Serializable m Day where
  serialize = serialize . Time.toModifiedJulianDay
  deserialize = Time.ModifiedJulianDay <$> deserialize

instance Serializable m DiffTime where
  serialize = serialize . toRational
  deserialize = fromRational <$> deserialize

instance Serializable m UniversalTime where
  serialize = serialize . Time.getModJulianDate
  deserialize = Time.ModJulianDate <$> deserialize

instance Serializable m UTCTime where
  serialize a = serialize (Time.utctDay a) *> serialize (Time.utctDayTime a)
  deserialize = Time.UTCTime <$> deserialize <*> deserialize

instance Serializable m NominalDiffTime where
  serialize = serialize . toRational
  deserialize = fromRational <$> deserialize

instance Serializable m TimeOfDay where
  serialize a = serialize (Time.todHour a) *> serialize (Time.todMin a) *> serialize (Time.todSec a)
  deserialize = Time.TimeOfDay <$> deserialize <*> deserialize <*> deserialize

instance Serializable m TimeZone where
  serialize a = 
    serialize (Time.timeZoneMinutes a) *>
    serialize (Time.timeZoneSummerOnly a) *>
    serialize (Time.timeZoneName a)
  deserialize = Time.TimeZone <$> deserialize <*> deserialize <*> deserialize

instance Serializable m LocalTime where
  serialize a = serialize (Time.localDay a) *> serialize (Time.localTimeOfDay a)
  deserialize = Time.LocalTime <$> deserialize <*> deserialize

instance Serializable m ZonedTime where
  serialize a = serialize (Time.zonedTimeToLocalTime a) *> serialize (Time.zonedTimeZone a)
  deserialize = Time.ZonedTime <$> deserialize <*> deserialize

instance Serializable m AbsoluteTime where
  serialize a = serialize $ Time.diffAbsoluteTime a Time.taiEpoch
  deserialize = toAbsoluteTime <$> deserialize
    where
      toAbsoluteTime dt = Time.addAbsoluteTime dt Time.taiEpoch


-- 'cereal' primitive instances wrappers:

instance Serializable m Char where serialize = put; deserialize = get
instance Serializable m Double where serialize = put; deserialize = get
instance Serializable m Float where serialize = put; deserialize = get
instance Serializable m Int where serialize = put; deserialize = get
instance Serializable m Int8 where serialize = put; deserialize = get
instance Serializable m Int16 where serialize = put; deserialize = get
instance Serializable m Int32 where serialize = put; deserialize = get
instance Serializable m Int64 where serialize = put; deserialize = get
instance Serializable m Integer where serialize = put; deserialize = get
instance Serializable m Word where serialize = put; deserialize = get
instance Serializable m Word8 where serialize = put; deserialize = get
instance Serializable m Word16 where serialize = put; deserialize = get
instance Serializable m Word32 where serialize = put; deserialize = get
instance Serializable m Word64 where serialize = put; deserialize = get
instance Serializable m () where serialize = put; deserialize = get
instance Serializable m ByteString where serialize = put; deserialize = get
instance Serializable m LazyByteString where serialize = put; deserialize = get
instance Serializable m IntSet where serialize = put; deserialize = get

put :: (Monad m, Applicative m, Cereal.Serialize a) => a -> Serialize m ()
put = Serialize.liftPut . Cereal.put

get :: (Monad m, Applicative m, Cereal.Serialize a) => Deserialize m a
get = Deserialize.liftGet Cereal.get


-- fixed 'cereal' instances:

instance Serializable m Bool where 
  serialize a = 
    Serialize.liftPut . Cereal.putWord8 $ case a of
      False -> 0
      True -> 1
  deserialize = do
    Deserialize.liftGet Cereal.getWord8 >>= \case
      0 -> return False
      1 -> return True
      _ -> Deserialize.throwError "Out of range"

instance Serializable m Ordering where 
  serialize a = 
    Serialize.liftPut . Cereal.putWord8 $ case a of
      LT -> 0
      EQ -> 1
      GT -> 2
  deserialize = do
    Deserialize.liftGet Cereal.getWord8 >>= \case
      0 -> return LT
      1 -> return EQ
      2 -> return GT
      _ -> Deserialize.throwError "Out of range"


-- Monoid wrappers instances:

instance (Serializable m a) => Serializable m (Dual a) where
  serialize = serialize . \case Dual a -> a
  deserialize = Dual <$> deserialize
  
instance Serializable m All where
  serialize = serialize . \case All a -> a
  deserialize = All <$> deserialize

instance Serializable m Any where
  serialize = serialize . \case Any a -> a
  deserialize = Any <$> deserialize

instance (Serializable m a) => Serializable m (Sum a) where
  serialize = serialize . \case Sum a -> a
  deserialize = Sum <$> deserialize

instance (Serializable m a) => Serializable m (Product a) where
  serialize = serialize . \case Product a -> a
  deserialize = Product <$> deserialize

instance (Serializable m a) => Serializable m (First a) where
  serialize = serialize . \case First a -> a
  deserialize = First <$> deserialize

instance (Serializable m a) => Serializable m (Last a) where
  serialize = serialize . \case Last a -> a
  deserialize = Last <$> deserialize


-- Composite instances:

instance (Serializable m a, Integral a) => Serializable m (Ratio a) where
  serialize a = serialize (numerator a) *> serialize (denominator a)
  deserialize = (%) <$> deserialize <*> deserialize

instance (Serializable m a) => Serializable m (Tree a) where
  serialize (Data.Tree.Node root sub) = serialize root *> serialize sub
  deserialize = Data.Tree.Node <$> deserialize <*> deserialize

instance (Serializable m a, Serializable m b) => Serializable m (Either a b) where
  serialize = \case
    Right a -> serialize True *> serialize a
    Left a -> serialize False *> serialize a
  deserialize = do
    deserialize >>= \case
      True -> Right <$> deserialize
      False -> Left <$> deserialize

instance (Serializable m a) => Serializable m (Maybe a) where
  serialize = \case 
    Just a -> serialize True >> serialize a
    Nothing -> serialize False
  deserialize = do
    z <- deserialize
    if z
      then deserialize >>= return . Just
      else return Nothing

instance (Serializable m a) => Serializable m [a] where
  serialize l = do
    serialize (length l)
    mapM_ serialize l
  deserialize = do
    n <- deserialize
    replicateM n deserialize

instance (Serializable m a) => Serializable m (Seq a) where
  serialize = serialize . toList
  deserialize = Data.Sequence.fromList <$> deserialize


-- Tuple instances:

instance (Serializable m a) => Serializable m (Identity a) where
  serialize = serialize . \case Identity a -> a
  deserialize = Identity <$> deserialize

instance (Serializable m a, Serializable m b) => Serializable m (a, b) where
  serialize (a, b) = serialize a *> serialize b
  deserialize = (,) <$> deserialize <*> deserialize
  
instance (Serializable m a, Serializable m b, Serializable m c) => Serializable m (a, b, c) where
  serialize (a, b, c) = serialize a *> serialize b *> serialize c
  deserialize = (,,) <$> deserialize <*> deserialize <*> deserialize
  
instance (Serializable m a, Serializable m b, Serializable m c, Serializable m d) => Serializable m (a, b, c, d) where
  serialize (a, b, c, d) = serialize a *> serialize b *> serialize c *> serialize d
  deserialize = (,,,) <$> deserialize <*> deserialize <*> deserialize <*> deserialize

instance (Serializable m a, Serializable m b, Serializable m c, Serializable m d, Serializable m e) => Serializable m (a, b, c, d, e) where
  serialize (a, b, c, d, e) = serialize a *> serialize b *> serialize c *> serialize d *> serialize e
  deserialize = (,,,,) <$> deserialize <*> deserialize <*> deserialize <*> deserialize <*> deserialize


-- 'containers' instances:

instance (Serializable m a, Ord a) => Serializable m (Set a) where
  serialize = serialize . Data.Set.toAscList
  deserialize = Data.Set.fromDistinctAscList <$> deserialize

instance (Serializable m a) => Serializable m (IntMap a) where
  serialize = serialize . Data.IntMap.toAscList
  deserialize = Data.IntMap.fromDistinctAscList <$> deserialize

instance (Serializable m a, Serializable m b, Ord a) => Serializable m (Map a b) where
  serialize = serialize . Data.Map.toAscList
  deserialize = Data.Map.fromDistinctAscList <$> deserialize


-- 'unordered-containers' instances:

instance (Serializable m a, Serializable m b, Hashable a, Eq a) => Serializable m (HashMap a b) where
  serialize = serialize . Data.HashMap.Lazy.toList
  deserialize = Data.HashMap.Lazy.fromList <$> deserialize

instance (Serializable m a, Hashable a, Eq a) => Serializable m (HashSet a) where
  serialize = serialize . Data.HashSet.toList
  deserialize = Data.HashSet.fromList <$> deserialize


-- 'array' instances:

instance (Serializable m e, Serializable m i, Ix i) => Serializable m (Array i e) where
  serialize = serializeArray
  deserialize = deserializeArray

instance (Serializable m e, Serializable m i, IArray UArray e, Ix i) => Serializable m (UArray i e) where
  serialize = serializeArray
  deserialize = deserializeArray

serializeArray :: (Monad m, Applicative m, Ix i, Serializable m e, Serializable m i, IArray a e) => a i e -> Serialize m ()
serializeArray a = do 
  serialize $ Data.Array.IArray.bounds a
  serialize $ Data.Array.IArray.elems a

deserializeArray :: (Monad m, Applicative m, Ix i, Serializable m e, Serializable m i, IArray a e) => Deserialize m (a i e)
deserializeArray = Data.Array.IArray.listArray <$> deserialize <*> deserialize


-- 'vector' instances:

instance (Serializable m a) => Serializable m (Vector a) where
  serialize = serializeVector
  deserialize = deserializeVector

instance (Serializable m a, Data.Vector.Primitive.Prim a) => Serializable m (PVector a) where
  serialize = serializeVector
  deserialize = deserializeVector

instance (Serializable m a, Data.Vector.Storable.Storable a) => Serializable m (SVector a) where
  serialize = serializeVector
  deserialize = deserializeVector

instance (Serializable m a, Data.Vector.Unboxed.Unbox a) => Serializable m (UVector a) where
  serialize = serializeVector
  deserialize = deserializeVector

serializeVector :: (Monad m, Applicative m, Data.Vector.Generic.Vector v a, Serializable m a) => v a -> Serialize m ()
serializeVector a = do
  serialize (Data.Vector.Generic.length a)
  Data.Vector.Generic.mapM_ serialize a

deserializeVector :: (Monad m, Applicative m, Data.Vector.Generic.Vector v a, Serializable m a) => Deserialize m (v a)
deserializeVector = do
  length <- deserialize
  Data.Vector.Generic.replicateM length deserialize


-- 'hashtables' instances:

instance ( Serializable (ST s) a, Serializable (ST s) b, Hashable a, Eq a ) => 
         Serializable (ST s) (Hashtables_Basic.HashTable s a b) where
  serialize = serializeHashTableST
  deserialize = deserializeHashTableST

instance ( Serializable (ST s) a, Serializable (ST s) b, Hashable a, Eq a ) => 
         Serializable (ST s) (Hashtables_Cuckoo.HashTable s a b) where
  serialize = serializeHashTableST
  deserialize = deserializeHashTableST

instance ( Serializable (ST s) a, Serializable (ST s) b, Hashable a, Eq a ) => 
         Serializable (ST s) (Hashtables_Linear.HashTable s a b) where
  serialize = serializeHashTableST
  deserialize = deserializeHashTableST

serializeHashTableST :: 
  (Hashtables_Class.HashTable t, Serializable (ST s) a, Serializable (ST s) b) => 
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
  (Hashtables_Class.HashTable t, Serializable (ST s) a, Serializable (ST s) b, Hashable a, Eq a) => 
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

instance ( Serializable (ST RealWorld) a, Serializable (ST RealWorld) b, Hashable a, Eq a ) => 
         Serializable IO (Hashtables_Basic.HashTable RealWorld a b) where
  serialize = Serialize.mapBase stToIO . serializeHashTableST
  deserialize = Deserialize.mapBase stToIO deserializeHashTableST

instance ( Serializable (ST RealWorld) a, Serializable (ST RealWorld) b, Hashable a, Eq a ) => 
         Serializable IO (Hashtables_Cuckoo.HashTable RealWorld a b) where
  serialize = Serialize.mapBase stToIO . serializeHashTableST
  deserialize = Deserialize.mapBase stToIO deserializeHashTableST

instance ( Serializable (ST RealWorld) a, Serializable (ST RealWorld) b, Hashable a, Eq a ) => 
         Serializable IO (Hashtables_Linear.HashTable RealWorld a b) where
  serialize = Serialize.mapBase stToIO . serializeHashTableST
  deserialize = Deserialize.mapBase stToIO deserializeHashTableST


-- Instances for mutable types from 'base':

instance (Serializable IO a) => Serializable IO (IORef a) where
  serialize = serialize <=< lift . readIORef
  deserialize = lift . newIORef =<< deserialize

instance (Serializable IO a) => Serializable IO (MVar a) where
  serialize = lift . readMVar >=> serialize
  deserialize = deserialize >>= lift . newMVar


-- Instances for 'stm':

instance (Serializable STM a) => Serializable STM (TVar a) where
  serialize = serialize <=< lift . readTVar
  deserialize = lift . newTVar =<< deserialize

instance (Serializable IO a) => Serializable IO (TVar a) where
  serialize = lift . atomically . readTVar >=> serialize
  deserialize = deserialize >>= lift . atomically . newTVar

