module Control.Arrow.Notation.EnvArrow where
import Prelude hiding (lookup)
import Control.Arrow
import Data.Maybe

import Control.Monad.Trans.Key
import Data.KeyMap

newtype Cage s x = Cage { open :: KeyMap s -> x }
  deriving (Functor, Applicative)

newtype EnvArrow a s = EnvArrow (a (KeyMap s) (KeyMap s))

instance Arrow a => Monoid (EnvArrow a x) where
  mempty = EnvArrow (arr id)
  EnvArrow l `mappend` EnvArrow r = EnvArrow (l >>> r)

toCage :: Key s a -> Cage s a
toCage k = Cage $ \env -> fromJust $ lookup k env

introEnv :: Arrow a => Key s x -> a x (KeyMap s)
introEnv k = arr $ \v -> insert k v empty

elimEnv :: Arrow a => Cage s x -> a (KeyMap s) x
elimEnv c = arr (open c)

extendEnv :: Arrow a => Key s x -> a (x, KeyMap s) (KeyMap s)
extendEnv k = arr . uncurry $ insert k

withEnv :: Arrow a => Cage s x -> a (KeyMap s) (x, KeyMap s)
withEnv c = dup >>> first (elimEnv c)
  where dup = arr $ \x -> (x, x)

toEnvArrow :: Arrow a => Cage s x -> Key s y -> a x y -> EnvArrow a s
toEnvArrow inC outK a = EnvArrow $ withEnv inC >>> first a >>> extendEnv outK

fromEnvArrow :: Arrow a => Key s x -> Cage s y -> EnvArrow a s -> a x y
fromEnvArrow inK outC (EnvArrow a) = introEnv inK >>> a >>> elimEnv outC
