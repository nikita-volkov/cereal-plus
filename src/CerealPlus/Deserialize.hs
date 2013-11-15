-- |
-- A monad-transformer over "Data.Serialize.Get".
module CerealPlus.Deserialize
  (
    Deserialize,
    runPartial,
    Result(..),
    liftGet,
    mapBase,
  )
  where

import CerealPlus.Prelude
import qualified Data.Serialize.Get as Cereal


-- | A deserialization monad transformer. 
newtype Deserialize m a = Deserialize { runPartial :: ByteString -> m (Result m a) }

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


data Result m a = 
  Fail Text ByteString |
  Partial (ByteString -> m (Result m a)) |
  Done a ByteString


liftGet :: Monad m => Cereal.Get a -> Deserialize m a
liftGet get = Deserialize $ \bs -> return $ convertResult $ Cereal.runGetPartial get bs 
  where
    convertResult r = case r of
      Cereal.Fail m bs -> Fail (packText m) bs
      Cereal.Partial cont -> Partial $ \bs -> return $ convertResult $ cont bs
      Cereal.Done a bs -> Done a bs

mapBase :: (Monad m, Monad m') => (forall b. m b -> m' b) -> Deserialize m a -> Deserialize m' a
mapBase mToM' (Deserialize runPartial) = Deserialize $ runPartialToRunPartial' runPartial
  where
    runPartialToRunPartial' runPartial = 
      mToM' . runPartial >=> \case
        Fail m bs -> return $ Fail m bs
        Partial runPartial' -> return $ Partial $ runPartialToRunPartial' runPartial'
        Done a bs -> return $ Done a bs

