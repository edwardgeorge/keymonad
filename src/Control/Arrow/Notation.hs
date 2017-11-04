module Control.Arrow.Notation where
import Prelude hiding (lookup)
import Control.Arrow
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Maybe

import Control.Monad.Trans.Key
import Data.KeyMap

newtype Cage s x = Cage { open :: KeyMap s -> x }
  deriving (Functor, Applicative)

newtype ArrowSyntax a s x = AS (WriterT (EnvArrow a s) (KeyM s) x)
  deriving (Functor, Applicative, Monad)

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

(-<) :: Arrow a => a x y -> (Cage s x -> ArrowSyntax a s (Cage s y))
a -< inC = AS $ do outK <- lift newKey
                   tell (toEnvArrow inC outK a)
                   return (toCage outK)

fromEnvArrow :: Arrow a => Key s x -> Cage s y -> EnvArrow a s -> a x y
fromEnvArrow inK outC (EnvArrow a) = introEnv inK >>> a >>> elimEnv outC

proc :: Arrow a => (forall s. Cage s x -> ArrowSyntax a s (Cage s y)) -> a x y
proc f = runKeyM $ do inK <- newKey
                      let AS m = f (toCage inK)
                      (outC, a) <- runWriterT m
                      return (fromEnvArrow inK outC a)
