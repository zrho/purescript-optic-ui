module OpticUI.Run
  ( AnimateE ()
  , animate
  ) where
--------------------------------------------------------------------------------
import Prelude
import OpticUI.Internal.VirtualDOM as VD
import OpticUI.Markup
import OpticUI.Core
import Control.Apply ((*>))
import Control.Monad (when)
import Control.Monad.Eff (Eff ())
import Control.Monad.Aff
import Control.Monad.State.Trans (StateT (), runStateT)
import Control.Monad.Free.Trans
import Control.Monad.Eff.Ref
import Data.Function (runFn2)
import Data.Exists (runExists)
import Data.Maybe (Maybe (..), maybe)
import Data.Nullable (toNullable, toMaybe)
import Data.Monoid (Monoid, mempty)
import Data.Tuple (Tuple (..))
import Data.Traversable (traverse)
import Data.Foldable (fold)
import Data.StrMap (StrMap (), empty)
import Data.Lens
import Data.Lens.At
import DOM (DOM ())
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.Event.EventTypes (load)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (HTMLElement (), htmlElementToNode, windowToEventTarget)
import DOM.HTML.Window (document)
import DOM.Node.Node (appendChild)
--------------------------------------------------------------------------------

type AnimateE eff = (dom :: DOM, ref :: REF | eff)
type Driver eff k = k -> Eff (AnimateE eff) Unit

animate
  :: forall s eff k. s
  -> UI (AnimateE eff) (Eff (AnimateE eff)) k Markup s s
  -> Eff (AnimateE eff) (Driver eff k)
animate s0 ui = do
  let vnode0 = VD.vtext ""
  let node0 = VD.createElement vnode0
  stR <- newRef
    ({ memo: { initializers: empty, finalizers: empty }
    , uiState: s0, generation: 0, node: node0, vnode: vnode0
    , signalHandler: \_ -> pure false
    } :: RunState (AnimateE eff) k s)
  let
    onEvent gen action = do
      st <- readRef stR
      when (gen == st.generation) $ action >>= \t -> step st { uiState = t }
    onSignal k = do
      st <- readRef stR
      void $ st.signalHandler k
    step st = case runUI ui st.uiState $ Handler (onEvent $ st.generation + 1) onSignal of
      Result sh v -> do
        Tuple vnode memo <- runStateT (buildVTree v) st.memo
        node <- VD.patch (VD.diff st.vnode vnode) st.node
        writeRef stR st
          { vnode = vnode, memo = memo, signalHandler = sh
          , node = node, generation = st.generation + 1 }
  onLoad do
    appendToBody node0
    readRef stR >>= step
  pure onSignal

--------------------------------------------------------------------------------

type Memo =
  { initializers :: StrMap VD.Props
  , finalizers :: StrMap VD.Props
  }

type RunState eff k s =
  { memo          :: Memo
  , uiState       :: s
  , generation    :: Int
  , node          :: HTMLElement
  , vnode         :: VD.VTree
  , signalHandler :: k -> Eff eff Boolean
  }

--------------------------------------------------------------------------------

-- | Build a vtree for a markup tree. Uses the memoized functions in the state
-- | where appropriate.
buildVTree :: forall m. (Monad m) => Markup -> StateT Memo m VD.VTree
buildVTree (Markup xs) = VD.vnode n "div" n mempty <$> traverse toVTree xs
  where n = toNullable Nothing

-- | Build a vtree for a markup node.
toVTree :: forall m. (Monad m) => Node -> StateT Memo m VD.VTree
toVTree (Text s) = return $ VD.vtext s
toVTree (Element ns tag props (Markup childs)) = do
  vprops <- fold <$> traverse toVProp props
  let key = lastOf (traversed <<< _KeyP) props
  tree <- traverse toVTree childs
  return $ VD.vnode (toNullable ns) tag (toNullable key) vprops tree

-- | Build a vprop for a markup property.
toVProp :: forall m. (Monad m) => Prop -> StateT Memo m VD.Props
toVProp (AttrP n v)          = pure $ runFn2 VD.attrProp n v
toVProp (HandlerP n ee)      = pure $ runEventHandler (\f -> runFn2 VD.handlerProp n f) ee
toVProp (PropP n ee)         = pure $ runExists (\(PropE e) -> runFn2 VD.prop n e) ee
toVProp (InitializerP key f) = findProp initializers key $ runInitializer (runFn2 VD.initializer key) f
toVProp (FinalizerP key f)   = findProp finalizers key $ runFinalizer (runFn2 VD.finalizer key) f
toVProp (KeyP key)           = pure mempty

-- | Looks for an initializer/finalizer with the same key. If it exists, it uses that one to avoid
-- | Virtual Dom removing and adding the node. If it doesn't exist, this function adds it to the
-- | correct StringMap in the state with the unique key
findProp :: forall m s p. (Monad m) => LensP s (StrMap p) -> UniqueStr -> p -> StateT s m p
findProp l key new = use (l <<< at key) >>= maybe ((l <<< at key ?= new) *> pure new) pure

initializers :: forall r. LensP { initializers :: StrMap VD.Props | r } (StrMap VD.Props)
initializers = lens _.initializers _ { initializers = _ }

finalizers :: forall r. LensP { finalizers :: StrMap VD.Props | r } (StrMap VD.Props)
finalizers = lens _.finalizers _ { finalizers = _ }

--------------------------------------------------------------------------------

onLoad :: forall eff. Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit
onLoad go = do
  w <- window
  let et = windowToEventTarget w
  addEventListener load (eventListener \_ -> go) false et

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = do
  w <- window
  d <- document w
  b <- toMaybe <$> body d
  case b of
    Nothing -> pure unit
    Just b' -> void $ appendChild (htmlElementToNode e) (htmlElementToNode b')
