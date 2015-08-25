module OpticUI.Internal.Pretext where

import OpticUI.Internal.Bazaar

import           Prelude
import           Data.List (List (..))
import qualified Data.List as L
import           Data.Tuple
import           Data.Const                  (Const (..), getConst)
import           Control.Comonad.Store.Class (ComonadStore, peek, pos)
import           Control.Extend
import           Control.Comonad
import           Data.Functor.Compose        (Compose (..), decompose)
import           Data.Identity               (Identity (..), runIdentity)
import           Optic.Core
import           Optic.Extended

--------------------------------------------------------------------------------

newtype Pretext a b t = Pretext (forall f. (Functor f) => (a -> f b) -> f t)

instance pretextFunctor :: Functor (Pretext a b) where
  map f (Pretext k) = Pretext (k >>> map f)

sellPretext :: forall a b. a -> Pretext a b b
sellPretext a = Pretext ($ a)

instance pretextExtend :: Extend (Pretext a a) where
  extend f (Pretext k)
    = map f $ decompose $ k
    $ Compose <<< map sellPretext <<< sellPretext

instance pretextComonad :: Comonad (Pretext a a) where
  extract (Pretext k) = runIdentity $ k Identity

instance pretextComonadStore :: ComonadStore a (Pretext a a) where
  peek a (Pretext k) = runIdentity $ k $ const $ Identity a
  pos (Pretext k) = getConst $ k Const

holesOf :: forall s t. TraversalP s t -> s -> L.List (Pretext t t s)
holesOf tr s = let bz = tr sell s in go (pins bz) (unsafeOuts bz) where
  go L.Nil _         = L.Nil
  go (L.Cons x xs) g = L.Cons
    (Pretext (\k -> map (\y -> g (L.Cons y xs)) (k x)))
    (go xs $ g <<< (L.Cons x))
