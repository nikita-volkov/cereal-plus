-- |
-- A monad-transformer over "Data.Serialize.Get".
module CerealPlus.DeserializeT
  (
    DeserializeT,
    runPartial,
    Result(..),
    liftGet,
    mapBase,
  )
  where

import CerealPlus.Prelude
import qualified Data.Serialize.Get as Cereal


-- | A deserialization monad transformer. 
-- Useful for mutable types, which live in monads like `IO`.
newtype DeserializeT m a = DeserializeT { runPartial :: ByteString -> m (Result m a) }

instance (Monad m) => Monad (DeserializeT m) where
  DeserializeT runA >>= aToDeserializeTB = DeserializeT $ \bs -> runA bs >>= aToMB where
    aToMB a = case a of
      Fail msg bs -> return $ Fail msg bs
      Partial cont -> return $ Partial $ \bs -> cont bs >>= aToMB
      Done a bs -> case aToDeserializeTB a of DeserializeT runB -> runB bs
  return a = DeserializeT $ \bs -> return $ Done a bs

instance MonadTrans DeserializeT where
  lift m = DeserializeT $ \bs -> m >>= \a -> return $ Done a bs

instance (MonadIO m) => MonadIO (DeserializeT m) where
  liftIO = lift . liftIO

instance (Monad m) => Applicative (DeserializeT m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Functor (DeserializeT m) where
  fmap = liftM


data Result m a = 
  Fail String ByteString |
  Partial (ByteString -> m (Result m a)) |
  Done a ByteString


liftGet :: Monad m => Cereal.Get a -> DeserializeT m a
liftGet get = DeserializeT $ \bs -> return $ convertResult $ Cereal.runGetPartial get bs 
  where
    convertResult r = case r of
      Cereal.Fail m bs -> Fail m bs
      Cereal.Partial cont -> Partial $ \bs -> return $ convertResult $ cont bs
      Cereal.Done a bs -> Done a bs

mapBase :: (Monad m, Monad m') => (forall b. m b -> m' b) -> DeserializeT m a -> DeserializeT m' a
mapBase mToM' (DeserializeT runPartial) = DeserializeT $ runPartialToRunPartial' runPartial
  where
    runPartialToRunPartial' runPartial = 
      mToM' . runPartial >=> \case
        Fail m bs -> return $ Fail m bs
        Partial runPartial' -> return $ Partial $ runPartialToRunPartial' runPartial'
        Done a bs -> return $ Done a bs

