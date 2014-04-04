-- |
-- A monad-transformer over "Data.Serialize.Get".
module CerealPlus.Deserialize
  (
    Deserialize,
    runPartial,
    Result(..),
    liftGet,
    mapBase,
    throwError,
  )
  where

import CerealPlus.Prelude
import qualified Data.Serialize.Get as Cereal
import qualified Control.Monad.Layer as Layers


-- | A deserialization monad transformer. 
newtype Deserialize m a = Deserialize { 
  -- | Run on a chunk of data and get a partial result.
  runPartial :: ByteString -> m (Result m a) 
}

instance (Monad m) => Monad (Deserialize m) where
  Deserialize runA >>= aToDeserializeTB = Deserialize $ \bs -> runA bs >>= aToMB where
    aToMB a = case a of
      Fail msg bs -> return $ Fail msg bs
      Partial cont -> return $ Partial $ \bs -> cont bs >>= aToMB
      Done a bs -> case aToDeserializeTB a of Deserialize runB -> runB bs
  return a = Deserialize $ \bs -> return $ Done a bs

instance MonadTrans Deserialize where
  lift m = Deserialize $ \bs -> m >>= \a -> return $ Done a bs

instance (MonadIO m) => MonadIO (Deserialize m) where
  liftIO = lift . liftIO

instance (Monad m) => Applicative (Deserialize m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Functor (Deserialize m) where
  fmap = liftM

instance (Monad m) => Layers.MonadTransFunctor (Deserialize m) where
  transMap = mapBase

instance (Monad m) => Layers.MonadTrans (Deserialize m) where
  type Outer (Deserialize m) = Deserialize
  transInvmap = const . Layers.transMap

instance (Monad m) => Layers.MonadLayerFunctor (Deserialize m) where
  layerMap = Layers.transMap

instance (Monad m) => Layers.MonadLayer (Deserialize m) where
  type Inner (Deserialize m) = m
  layerInvmap = const . Layers.layerMap
  layer = lift


-- | A partial result of deserialization.
data Result m a = 
  -- | A message describing the deserialization failure and a remaining chunk.
  Fail Text ByteString |
  -- | A continuation function, which should be supplied with the next chunk.
  Partial (ByteString -> m (Result m a)) |
  -- | A deserialized data structure and a remaining chunk.
  Done a ByteString

-- | Run a `Cereal.Get` action of the \"cereal\" library.
liftGet :: Monad m => Cereal.Get a -> Deserialize m a
liftGet get = Deserialize $ \bs -> return $ convertResult $ Cereal.runGetPartial get bs 
  where
    convertResult r = case r of
      Cereal.Fail m bs -> Fail (packText m) bs
      Cereal.Partial cont -> Partial $ \bs -> return $ convertResult $ cont bs
      Cereal.Done a bs -> Done a bs

-- | Change the base monad. Same as `Layers.transMap` of the \"layers\" library.
mapBase :: (Monad m, Monad m') => (forall b. m b -> m' b) -> Deserialize m a -> Deserialize m' a
mapBase mToM' = \(Deserialize runPartial) -> Deserialize $ runPartialToRunPartial' runPartial
  where
    runPartialToRunPartial' runPartial = 
      mToM' . runPartial >=> \case
        Fail m bs -> return $ Fail m bs
        Partial runPartial' -> return $ Partial $ runPartialToRunPartial' runPartial'
        Done a bs -> return $ Done a bs

-- | 
-- Fail with a message.
-- 
-- Since there's no consensus on how to implement 'catchError' of 'MonadError', 
-- we'll go with just this function.
-- 
-- It is not implemented as 'fail' because 'fail' is pure evil.
throwError :: (Monad m) => Text -> Deserialize m a
throwError t = Deserialize $ \bs -> return $ Fail t bs
