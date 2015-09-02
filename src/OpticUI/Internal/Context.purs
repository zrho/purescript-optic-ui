module OpticUI.Internal.Context where
--------------------------------------------------------------------------------
import           Prelude
import           Control.Comonad.Store.Class (ComonadStore, peek, pos)
import           Control.Extend
import           Control.Comonad
import           Optic.Core
--------------------------------------------------------------------------------

data Context a b t = Context (b -> t) a

instance contextFunctor :: Functor (Context a b) where
  map f (Context g x) = Context (f <<< g) x

instance contextExtend :: Extend (Context a a) where
  extend f (Context g x) = Context (f <<< Context g) x

instance contextComonad :: Comonad (Context a a) where
  extract (Context g x) = g x

instance contextComonadStore :: ComonadStore a (Context a a) where
  peek x (Context g _) = g x
  pos (Context _ x) = x

fromLens :: forall s t a b. Lens s t a b -> s -> Context a b t
fromLens l = l (Context id)
