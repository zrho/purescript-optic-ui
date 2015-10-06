module OpticUI.Internal.VirtualDOM where
--------------------------------------------------------------------------------
import Prelude
import Data.Monoid       (Monoid)
import Control.Monad.Eff (Eff ())
import Data.Function     (Fn2 (), runFn2)
import DOM               (DOM ())
import DOM.HTML.Types    (HTMLElement ())
import Data.Nullable     (Nullable())
--------------------------------------------------------------------------------

data VTree
data Patch
data Props

--------------------------------------------------------------------------------
-- DOM
--------------------------------------------------------------------------------

foreign import createElement
  :: VTree -> HTMLElement

foreign import diff
  :: VTree -> VTree -> Patch

foreign import patch
  :: forall eff. Patch -> HTMLElement -> Eff (dom :: DOM | eff) HTMLElement

--------------------------------------------------------------------------------
-- Elements
--------------------------------------------------------------------------------

foreign import vtext :: String -> VTree

foreign import vnode
  :: Nullable String
  -> String
  -> Nullable String
  -> Props
  -> Array VTree
  -> VTree

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

foreign import prop :: forall a. Fn2 String a Props
foreign import attrProp :: Fn2 String String Props
foreign import handlerProp :: forall eff e. Fn2 String (e -> Eff eff Unit) Props
foreign import concatProps :: Fn2 Props Props Props
foreign import emptyProps  :: Props
foreign import initializer :: forall eff. Fn2 String (HTMLElement -> Eff eff Unit) Props
foreign import finalizer   :: forall eff. Fn2 String (HTMLElement -> Eff eff Unit) Props

instance semigroupProps :: Semigroup Props where
  append = runFn2 concatProps

instance monoidProps :: Monoid Props where
  mempty = emptyProps
