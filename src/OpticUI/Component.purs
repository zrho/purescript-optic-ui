module OpticUI.Component where
--------------------------------------------------------------------------------
import Prelude
import Data.Foldable
import Data.Traversable
import Control.Alt
import Control.Plus
import Control.Alternative
import Control.MonadPlus

-- | Product of a view and a subscription functor that make up a component.
data Component f g a = Component (f a) (g a)

instance functorComponent :: (Functor f, Functor g) => Functor (Component f g) where
  map f (Component fa ga) = Component (map f fa) (map f ga)

instance foldableComponent :: (Foldable f, Foldable g) => Foldable (Component f g) where
  foldr f z (Component fa ga) = foldr f (foldr f z ga) fa
  foldl f z (Component fa ga) = foldl f (foldl f z fa) ga
  foldMap f (Component fa ga) = foldMap f fa <> foldMap f ga

instance traversableComponent :: (Traversable f, Traversable g) => Traversable (Component f g) where
  traverse f (Component fa ga) = Component <$> traverse f fa <*> traverse f ga
  sequence (Component fa ga) = Component <$> sequence fa <*> sequence ga

instance applyComponent :: (Apply f, Apply g) => Apply (Component f g) where
  apply (Component ff gf) (Component fa ga) = Component (apply ff fa) (apply gf ga)

instance applicativeComponent :: (Applicative f, Applicative g) => Applicative (Component f g) where
  pure a = Component (pure a) (pure a)

instance bindComponent :: (Bind f, Bind g) => Bind (Component f g) where
  bind (Component fa ga) f = Component
    (fa >>= f >>> \(Component fb _) -> fb)
    (ga >>= f >>> \(Component _ gb) -> gb)

instance monadComponent :: (Monad f, Monad g) => Monad (Component f g)

instance altComponent :: (Alt f, Alt g) => Alt (Component f g) where
  alt (Component fa ga) (Component fb gb) = Component (alt fa fb) (alt ga gb)

instance plusComponent :: (Plus f, Plus g) => Plus (Component f g) where
  empty = Component empty empty

instance alternativeComponent :: (Alternative f, Alternative g) => Alternative (Component f g) where

instance monadPlusComponent :: (MonadPlus f, MonadPlus g) => MonadPlus (Component f g) where

