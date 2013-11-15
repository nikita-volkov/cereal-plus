-- |
-- A monad-transformer over "Data.Serialize.Put".
module CerealPlus.SerializeT
  (
    SerializeT,
    run,
    runLazy,
    exec,
    execLazy,
    liftPut,
    mapBase,
  )
  where

import CerealPlus.Prelude
import qualified Data.Serialize.Put as Cereal


-- | A serialization monad transformer.
-- Useful for mutable types, which live in monads like `IO`.
newtype SerializeT m a = SerializeT (WriterT (PutM' ()) m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadPlus, Alternative)

newtype PutM' a = PutM' (Cereal.PutM a)
  deriving (Functor, Applicative, Monad)

-- | Required for 'WriterT'
instance Monoid (PutM' ()) where
  mempty = return ()
  mappend a b = a >> b


run :: Monad m => SerializeT m a -> m (a, ByteString)
run (SerializeT w) = do
  (a, PutM' putM) <- runWriterT w
  return (a, Cereal.runPut putM)

runLazy :: Monad m => SerializeT m a -> m (a, LazyByteString)
runLazy (SerializeT w) = do
  (a, PutM' putM) <- runWriterT w
  return (a, Cereal.runPutLazy putM)

exec :: Monad m => SerializeT m a -> m ByteString
exec (SerializeT w) = do
  PutM' putM <- execWriterT w
  return $ Cereal.runPut putM

execLazy :: Monad m => SerializeT m a -> m LazyByteString
execLazy (SerializeT w) = do
  PutM' putM <- execWriterT w
  return $ Cereal.runPutLazy putM


liftPut :: Monad m => Cereal.Put -> SerializeT m ()
liftPut put = SerializeT $ tell $ PutM' put

mapBase :: (forall b. m b -> m' b) -> SerializeT m a -> SerializeT m' a
mapBase f (SerializeT writer) = SerializeT $ mapWriterT f writer
