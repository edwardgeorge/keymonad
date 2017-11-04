module Data.KeyMap where
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Type.Equality

import Control.Monad.Trans.Key
import Data.NameSupply

data Box s where
  Lock :: Key s a -> a -> Box s

instance Eq (Box s) where
  Lock (Key k) _ == Lock (Key k') _ = k == k'

instance Ord (Box s) where
  Lock (Key k) _ `compare` Lock (Key k') _ = k `compare` k'

unlock :: Key s a -> Box s -> Maybe a
unlock k (Lock k' x) = (\Refl -> x) <$> testKeyEquality k k'

newtype NameIn s = Scoped Name
  deriving (Eq, Ord, Show)

newtype KeyMap s = KM (Map (NameIn s) (Box s))
  deriving (Monoid)

empty :: KeyMap s
empty = KM mempty

insert :: Key s a -> a -> KeyMap s -> KeyMap s
insert k@(Key n) v (KM m) = KM (Map.insert (Scoped n) (Lock k v) m)

lookup :: Key s a -> KeyMap s -> Maybe a
lookup k@(Key n) (KM m) = Map.lookup (Scoped n) m >>= unlock k

record :: Monad m => a -> KeyMap s -> KeyT s m (Key s a, KeyMap s)
record a m = do
  key <- newKeyT
  let n = insert key a m
  return (key, n)
