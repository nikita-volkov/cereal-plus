-- |
-- A monad-transformer over "Data.Serialize.Put".
module CerealPlus.SerializeT
  (
    Serialize,
    SerializeT,
    run,
    liftPut,
  )
  where

import CerealPlus.Prelude hiding (Result(..))
import qualified Data.Serialize.Put as Cereal


-- | A serialization monad used for pure data.
type Serialize = SerializeT Identity

-- | A serialization monad transformer.
-- Useful for mutable types, which live in monads like `IO`.
newtype SerializeT m a = SerializeT (WriterT (PutM' ()) m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadPlus, Alternative)

run :: Monad m => SerializeT m a -> m (Cereal.PutM a)
run (SerializeT writer) = do
  (r, put) <- runWriterT writer
  case put of PutM' put -> return $ put >> return r

liftPut :: Monad m => Cereal.Put -> SerializeT m ()
liftPut put = SerializeT $ tell $ PutM' put


newtype PutM' a = PutM' (Cereal.PutM a)
  deriving (Functor, Applicative, Monad)

-- | Required for 'WriterT'
instance Monoid (PutM' ()) where
  mempty = return ()
  mappend a b = a >> b

