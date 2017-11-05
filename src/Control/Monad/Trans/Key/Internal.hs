module Control.Monad.Trans.Key.Internal where
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import Data.NameSupply

newtype KeyT s m a = KeyT { getKeyT :: ReaderT NameSupply m a }
  deriving (Functor, MonadTrans)

instance Monad m => Applicative (KeyT s m) where
  pure = KeyT . pure
  (<*>) = ap

instance Monad m => Monad (KeyT s m) where
  KeyT f >>= g =
    KeyT . ReaderT $ \s -> do
      let (sl, sr) = split s
      r <- runReaderT f sl
      runReaderT (getKeyT $ g r) sr

newtype Key s a = Key Name
  deriving (Eq, Ord, Show)
