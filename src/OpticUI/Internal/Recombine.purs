module OpticUI.Internal.Recombine (Recombine (..), toRecombine, fromRecombine) where
--------------------------------------------------------------------------------
import Prelude
import Control.Alt
import Control.Plus

-- | Data type for making a profunctor represented by an `Alt` functor
-- | temporarily into a profunctor represented by an `Applicative`.
data Recombine f a = Recombine a (f a)

instance functorRecombine :: (Functor f) => Functor (Recombine f) where
  map f (Recombine a fa) = Recombine (f a) (map f fa)

instance applyRecombine :: (Alt f) => Apply (Recombine f) where
  apply (Recombine h fh) (Recombine a fa) = Recombine (h a) (map ($ a) fh `alt` map h fa)

instance applicativeRecombine :: (Plus f) => Applicative (Recombine f) where
  pure a = Recombine a empty

-- | Convert from a profunctor represnted by `f` to one represented by `Recombine f`.
toRecombine :: forall f a. (a -> f a) -> a -> Recombine f a
toRecombine f a = Recombine a (f a)

-- | Convert from a profunctor represented by `Recombine f` back to one
-- | represented by `f`.
fromRecombine :: forall f a b. (a -> Recombine f b) -> a -> f b
fromRecombine f a = case f a of
  Recombine _ fa -> fa
