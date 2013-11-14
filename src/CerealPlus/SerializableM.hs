module CerealPlus.SerializableM (SerializableM(..)) where

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


class SerializableM a m where
  serializeT :: a -> SerializeT m ()
  deserializeT :: DeserializeT m a


instance (Applicative m, Monad m, HasResolution a, Fractional (Fixed a)) => SerializableM (Fixed a) m where
  serializeT = serializeT . toRational
  deserializeT = fromRational <$> deserializeT


-- 'text' instances:

instance (Applicative m, Monad m) => SerializableM Text m where
  serializeT = serializeT . Text.encodeUtf8
  deserializeT = Text.decodeUtf8 <$> deserializeT

instance (Applicative m, Monad m) => SerializableM LazyText m where
  serializeT = serializeT . LazyText.encodeUtf8
  deserializeT = LazyText.decodeUtf8 <$> deserializeT


-- 'time' instances:

instance (Applicative m, Monad m) => SerializableM Day m where
  serializeT = serializeT . Time.toModifiedJulianDay
  deserializeT = Time.ModifiedJulianDay <$> deserializeT

instance (Applicative m, Monad m) => SerializableM DiffTime m where
  serializeT = serializeT . toRational
  deserializeT = fromRational <$> deserializeT

instance (Applicative m, Monad m) => SerializableM UniversalTime m where
  serializeT = serializeT . Time.getModJulianDate
  deserializeT = Time.ModJulianDate <$> deserializeT

instance (Applicative m, Monad m) => SerializableM UTCTime m where
  serializeT a = serializeT (Time.utctDay a) *> serializeT (Time.utctDayTime a)
  deserializeT = Time.UTCTime <$> deserializeT <*> deserializeT

instance (Applicative m, Monad m) => SerializableM NominalDiffTime m where
  serializeT = serializeT . toRational
  deserializeT = fromRational <$> deserializeT

instance (Applicative m, Monad m) => SerializableM TimeOfDay m where
  serializeT a = serializeT (Time.todHour a) *> serializeT (Time.todMin a) *> serializeT (Time.todSec a)
  deserializeT = Time.TimeOfDay <$> deserializeT <*> deserializeT <*> deserializeT

instance (Applicative m, Monad m) => SerializableM TimeZone m where
  serializeT a = 
    serializeT (Time.timeZoneMinutes a) *>
    serializeT (Time.timeZoneSummerOnly a) *>
    serializeT (Time.timeZoneName a)
  deserializeT = Time.TimeZone <$> deserializeT <*> deserializeT <*> deserializeT

instance (Applicative m, Monad m) => SerializableM LocalTime m where
  serializeT a = serializeT (Time.localDay a) *> serializeT (Time.localTimeOfDay a)
  deserializeT = Time.LocalTime <$> deserializeT <*> deserializeT

instance (Applicative m, Monad m) => SerializableM ZonedTime m where
  serializeT a = serializeT (Time.zonedTimeToLocalTime a) *> serializeT (Time.zonedTimeZone a)
  deserializeT = Time.ZonedTime <$> deserializeT <*> deserializeT

instance (Applicative m, Monad m) => SerializableM AbsoluteTime m where
  serializeT a = serializeT $ Time.diffAbsoluteTime a Time.taiEpoch
  deserializeT = toAbsoluteTime <$> deserializeT
    where
      toAbsoluteTime dt = Time.addAbsoluteTime dt Time.taiEpoch


-- 'cereal' primitive instances wrappers:

instance (Applicative m, Monad m) => SerializableM Bool m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM Char m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM Double m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM Float m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM Int m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM Int8 m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM Int16 m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM Int32 m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM Int64 m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM Integer m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM Ordering m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM Word m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM Word8 m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM Word16 m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM Word32 m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM Word64 m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM () m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM ByteString m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM LazyByteString m where serializeT = put; deserializeT = get
instance (Applicative m, Monad m) => SerializableM IntSet m where serializeT = put; deserializeT = get

put :: (Applicative m, Monad m, Cereal.Serialize a) => a -> SerializeT m ()
put = SerializeT.liftPut . Cereal.put

get :: (Applicative m, Monad m, Cereal.Serialize a) => DeserializeT m a
get = DeserializeT.liftGet Cereal.get


-- Monoid wrappers instances:

instance (Applicative m, Monad m, SerializableM a m) => SerializableM (Dual a) m where
  serializeT = serializeT . \case Dual a -> a
  deserializeT = Dual <$> deserializeT
  
instance (Applicative m, Monad m) => SerializableM All m where
  serializeT = serializeT . \case All a -> a
  deserializeT = All <$> deserializeT

instance (Applicative m, Monad m) => SerializableM Any m where
  serializeT = serializeT . \case Any a -> a
  deserializeT = Any <$> deserializeT

instance (Applicative m, Monad m, SerializableM a m) => SerializableM (Sum a) m where
  serializeT = serializeT . \case Sum a -> a
  deserializeT = Sum <$> deserializeT

instance (Applicative m, Monad m, SerializableM a m) => SerializableM (Product a) m where
  serializeT = serializeT . \case Product a -> a
  deserializeT = Product <$> deserializeT

instance (Applicative m, Monad m, SerializableM a m) => SerializableM (First a) m where
  serializeT = serializeT . \case First a -> a
  deserializeT = First <$> deserializeT

instance (Applicative m, Monad m, SerializableM a m) => SerializableM (Last a) m where
  serializeT = serializeT . \case Last a -> a
  deserializeT = Last <$> deserializeT


-- Composite instances:

instance (Applicative m, Monad m, SerializableM a m, Integral a) => SerializableM (Ratio a) m where
  serializeT a = serializeT (numerator a) *> serializeT (denominator a)
  deserializeT = (%) <$> deserializeT <*> deserializeT

instance (Applicative m, Monad m, SerializableM a m) => SerializableM (Tree a) m where
  serializeT (Data.Tree.Node root sub) = serializeT root *> serializeT sub
  deserializeT = Data.Tree.Node <$> deserializeT <*> deserializeT

instance (Applicative m, Monad m, SerializableM a m, SerializableM b m) => SerializableM (Either a b) m where
  serializeT = \case
    Right a -> serializeT True *> serializeT a
    Left a -> serializeT False *> serializeT a
  deserializeT = do
    deserializeT >>= \case
      True -> Right <$> deserializeT
      False -> Left <$> deserializeT

instance (Applicative m, Monad m, SerializableM a m) => SerializableM (Maybe a) m where
  serializeT = \case 
    Just a -> serializeT True >> serializeT a
    Nothing -> serializeT False
  deserializeT = do
    z <- deserializeT
    if z
      then deserializeT >>= return . Just
      else return Nothing

instance (Applicative m, Monad m, SerializableM a m) => SerializableM [a] m where
  serializeT l = do
    serializeT (length l)
    mapM_ serializeT l
  deserializeT = do
    n <- deserializeT
    replicateM n deserializeT

instance (Applicative m, Monad m, SerializableM a m) => SerializableM (Seq a) m where
  serializeT = serializeT . toList
  deserializeT = Data.Sequence.fromList <$> deserializeT


-- Tuple instances:

instance (Applicative m, Monad m, SerializableM a m) => SerializableM (Identity a) m where
  serializeT = serializeT . \case Identity a -> a
  deserializeT = Identity <$> deserializeT

instance (Applicative m, Monad m, SerializableM a m, SerializableM b m) => SerializableM (a, b) m where
  serializeT (a, b) = serializeT a *> serializeT b
  deserializeT = (,) <$> deserializeT <*> deserializeT
  
instance (Applicative m, Monad m, SerializableM a m, SerializableM b m, SerializableM c m) => SerializableM (a, b, c) m where
  serializeT (a, b, c) = serializeT a *> serializeT b *> serializeT c
  deserializeT = (,,) <$> deserializeT <*> deserializeT <*> deserializeT
  
instance (Applicative m, Monad m, SerializableM a m, SerializableM b m, SerializableM c m, SerializableM d m) => SerializableM (a, b, c, d) m where
  serializeT (a, b, c, d) = serializeT a *> serializeT b *> serializeT c *> serializeT d
  deserializeT = (,,,) <$> deserializeT <*> deserializeT <*> deserializeT <*> deserializeT

instance (Applicative m, Monad m, SerializableM a m, SerializableM b m, SerializableM c m, SerializableM d m, SerializableM e m) => SerializableM (a, b, c, d, e) m where
  serializeT (a, b, c, d, e) = serializeT a *> serializeT b *> serializeT c *> serializeT d *> serializeT e
  deserializeT = (,,,,) <$> deserializeT <*> deserializeT <*> deserializeT <*> deserializeT <*> deserializeT


-- 'containers' instances:

instance (Applicative m, Monad m, SerializableM a m, Ord a) => SerializableM (Set a) m where
  serializeT = serializeT . Data.Set.toAscList
  deserializeT = Data.Set.fromDistinctAscList <$> deserializeT

instance (Applicative m, Monad m, SerializableM a m) => SerializableM (IntMap a) m where
  serializeT = serializeT . Data.IntMap.toAscList
  deserializeT = Data.IntMap.fromDistinctAscList <$> deserializeT

instance (Applicative m, Monad m, SerializableM a m, SerializableM b m, Ord a) => SerializableM (Map a b) m where
  serializeT = serializeT . Data.Map.toAscList
  deserializeT = Data.Map.fromDistinctAscList <$> deserializeT


-- 'unordered-containers' instances:

instance (Applicative m, Monad m, SerializableM a m, SerializableM b m, Hashable a, Eq a) => SerializableM (HashMap a b) m where
  serializeT = serializeT . Data.HashMap.Lazy.toList
  deserializeT = Data.HashMap.Lazy.fromList <$> deserializeT

instance (Applicative m, Monad m, SerializableM a m, Hashable a, Eq a) => SerializableM (HashSet a) m where
  serializeT = serializeT . Data.HashSet.toList
  deserializeT = Data.HashSet.fromList <$> deserializeT


-- 'array' instances:

instance (Applicative m, Monad m, SerializableM e m, SerializableM i m, Ix i) => SerializableM (Array i e) m where
  serializeT = serializeArray
  deserializeT = deserializeArray

instance (Applicative m, Monad m, SerializableM e m, SerializableM i m, IArray UArray e, Ix i) => SerializableM (UArray i e) m where
  serializeT = serializeArray
  deserializeT = deserializeArray

serializeArray :: (Applicative m, Monad m, Ix i, SerializableM e m, SerializableM i m, IArray a e) => a i e -> SerializeT m ()
serializeArray a = do 
  serializeT $ Data.Array.IArray.bounds a
  serializeT $ Data.Array.IArray.elems a

deserializeArray :: (Applicative m, Monad m, Ix i, SerializableM e m, SerializableM i m, IArray a e) => DeserializeT m (a i e)
deserializeArray = Data.Array.IArray.listArray <$> deserializeT <*> deserializeT


-- 'vector' instances:

instance (Applicative m, Monad m, SerializableM a m) => SerializableM (Vector a) m where
  serializeT = serializeVector
  deserializeT = deserializeVector

instance (Applicative m, Monad m, SerializableM a m, Data.Vector.Primitive.Prim a) => SerializableM (PVector a) m where
  serializeT = serializeVector
  deserializeT = deserializeVector

instance (Applicative m, Monad m, SerializableM a m, Data.Vector.Storable.Storable a) => SerializableM (SVector a) m where
  serializeT = serializeVector
  deserializeT = deserializeVector

instance (Applicative m, Monad m, SerializableM a m, Data.Vector.Unboxed.Unbox a) => SerializableM (UVector a) m where
  serializeT = serializeVector
  deserializeT = deserializeVector

serializeVector :: (Applicative m, Monad m, Data.Vector.Generic.Vector v a, SerializableM a m) => v a -> SerializeT m ()
serializeVector a = do
  serializeT (Data.Vector.Generic.length a)
  Data.Vector.Generic.mapM_ serializeT a

deserializeVector :: (Applicative m, Monad m, Data.Vector.Generic.Vector v a, SerializableM a m) => DeserializeT m (v a)
deserializeVector = do
  length <- deserializeT
  Data.Vector.Generic.replicateM length deserializeT
