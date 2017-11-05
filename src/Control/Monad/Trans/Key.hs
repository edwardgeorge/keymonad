module Control.Monad.Trans.Key
  (
  -- * Key Monad
    KeyM
  , KeyT(..)
  , Key(..)
  , newKey
  , newKeyT
  , runKeyM
  , runKeyT
  , testKeyEquality
  ) where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Data.Type.Equality
import Unsafe.Coerce

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

type KeyM s = KeyT s Identity

newtype Key s a = Key Name
  deriving (Eq, Ord, Show)

newKeyT :: Applicative m => KeyT s m (Key s a)
newKeyT = KeyT . ReaderT $ pure . Key . supplyName

newKey :: KeyM s (Key s a)
newKey = newKeyT

runKeyT :: (forall s. KeyT s m a) -> m a
runKeyT (KeyT f) = runReaderT f newNameSupply

runKeyM :: (forall s. KeyM s a) -> a
runKeyM s = runIdentity $ runKeyT s

testKeyEquality :: Key s a -> Key s b -> Maybe (a :~: b)
testKeyEquality (Key l) (Key r)
  | l == r = Just (unsafeCoerce Refl)
  | otherwise = Nothing
