module Control.Monad.Trans.Key
  (
  -- * Key Monad
    KeyM
  , KeyT
  , Key
  , newKey
  , newKeyT
  , runKeyM
  , runKeyT
  , testKeyEquality
  ) where

import Control.Monad
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import Data.Type.Equality
import Unsafe.Coerce

import Control.Monad.Trans.Key.Internal
import Data.NameSupply

type KeyM s = KeyT s Identity

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
