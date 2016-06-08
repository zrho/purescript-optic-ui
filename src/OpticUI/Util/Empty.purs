module OpticUI.Util.Empty where
------------------------------------------------------------------------------------------
import Prelude
import Control.Alt
import Control.Plus
import Control.Alternative
import Control.MonadPlus
import Data.Foldable
import Data.Traversable
import Data.Monoid

data Empty a = Empty

instance eqEmpty :: Eq (Empty a) where
  eq _ _ = true

instance ordEmpty :: Ord (Empty a) where
  compare _ _ = EQ

instance showEmpty :: Show (Empty a) where
  show _ = "Empty"

instance functorEmpty :: Functor Empty where
  map _ _ = Empty

instance foldableEmpty :: Foldable Empty where
  foldl _ z _ = z
  foldr _ z _ = z
  foldMap _ _ = mempty

instance traversableEmpty :: Traversable Empty where
  traverse _ _ = pure Empty
  sequence _ = pure Empty

instance semigroupEmpty :: Semigroup (Empty a) where
  append _ _ = Empty

instance monoidEmpty :: Monoid (Empty a) where
  mempty = Empty

instance applyEmpty :: Apply Empty where
  apply _ _ = Empty

instance applicativeEmpty :: Applicative Empty where
  pure _ = Empty

instance altEmpty :: Alt Empty where
  alt _ _ = Empty

instance plusEmpty :: Plus Empty where
  empty = Empty

instance bindEmpty :: Bind Empty where
  bind _ _ = Empty

instance alternativeEmpty :: Alternative Empty where
instance monadEmpty :: Monad Empty where
instance monadPlusEmpty :: MonadPlus Empty where
