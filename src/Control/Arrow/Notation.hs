module Control.Arrow.Notation where
import Control.Arrow
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer

import Control.Arrow.Notation.EnvArrow
import Control.Monad.Trans.Key

newtype ArrowSyntax a s x = AS (WriterT (EnvArrow a s) (KeyM s) x)
  deriving (Functor, Applicative, Monad)

(-<) :: Arrow a => a x y -> (Cage s x -> ArrowSyntax a s (Cage s y))
a -< inC =
  AS $ do
    outK <- lift newKey
    tell (toEnvArrow inC outK a)
    return (toCage outK)

proc :: Arrow a => (forall s. Cage s x -> ArrowSyntax a s (Cage s y)) -> a x y
proc f =
  runKeyM $ do
    inK <- newKey
    let AS m = f (toCage inK)
    (outC, a) <- runWriterT m
    return (fromEnvArrow inK outC a)
