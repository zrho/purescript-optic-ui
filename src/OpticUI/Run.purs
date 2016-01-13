module OpticUI.Run where
--------------------------------------------------------------------------------
import Prelude
import OpticUI.Internal.VirtualDOM as VD
import OpticUI.Markup
import OpticUI.Core
import Control.Apply ((*>))
import Control.Monad (when)
import Control.Monad.Eff (Eff ())
import Control.Monad.Eff.Ref (REF (), Ref (), newRef, readRef, writeRef)
import Control.Monad.State.Trans (StateT (), runStateT)
import Control.Monad.State.Class (get)
import Control.Monad.Trans (lift)
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
import Data.Lens.Zoom
import DOM (DOM ())
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.Event.EventTypes (load)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (HTMLElement (), htmlElementToNode, windowToEventTarget)
import DOM.HTML.Window (document)
import DOM.Node.Node (appendChild)
--------------------------------------------------------------------------------

type Driver eff s = (s -> Eff eff s) -> Eff eff Unit
type AnimateE eff = (dom :: DOM, ref :: REF | eff)

animate
  :: forall s eff. s
  -> UI (AnimateE eff) (Eff (AnimateE eff)) Markup s s
  -> Eff (AnimateE eff) (Driver (AnimateE eff) s)
animate s0 ui = do
  let vnode0 = VD.vtext ""
  let node0 = VD.createElement vnode0
  stR <- newRef (
    { memo: { initializers: empty, finalizers: empty }
    , uiState: s0, generation: 0, node: node0, vnode: vnode0
    } :: RunState s)
  let
    step s = do
      rs <- get
      markup <- lift $ runUI ui s $ Handler (>>= (refresh rs.generation))
      vnodeNew <- zoom memo (buildVTree markup)
      nodeNew <- lift $ VD.patch (VD.diff rs.vnode vnodeNew) rs.node
      vnode .= vnodeNew
      uiState .= s
      node .= nodeNew
    checkGen gen go = do
      cur <- use generation
      when (gen == cur) (generation += 1 *> go)
    refresh gen s = stateRef stR $ checkGen gen $ step s
    driver f = stateRef stR do
      generation += 1
      use uiState >>= f >>> lift >>= step
  onLoad do
    appendToBody node0
    refresh 0 s0
  return driver

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

--------------------------------------------------------------------------------

type Memo =
  { initializers :: StrMap VD.Props
  , finalizers :: StrMap VD.Props
  }

type RunState s =
  { memo       :: Memo
  , uiState    :: s
  , generation :: Int
  , node       :: HTMLElement
  , vnode      :: VD.VTree
  }

-- We really do need automatic generation of lenses. Or first class labels;
-- that would be even more awesome!

initializers :: forall r. LensP { initializers :: StrMap VD.Props | r } (StrMap VD.Props)
initializers = lens _.initializers _ { initializers = _ }

finalizers :: forall r. LensP { finalizers :: StrMap VD.Props | r } (StrMap VD.Props)
finalizers = lens _.finalizers _ { finalizers = _ }

memo :: forall r. LensP { memo :: Memo | r } Memo
memo = lens _.memo _ { memo = _ }

uiState :: forall r s. LensP { uiState :: s | r } s
uiState = lens _.uiState _ { uiState = _ }

generation :: forall r. LensP { generation :: Int | r } Int
generation = lens _.generation _ { generation = _ }

node :: forall r. LensP { node :: HTMLElement | r } HTMLElement
node = lens _.node _ { node = _ }

vnode :: forall r. LensP { vnode :: VD.VTree | r } VD.VTree
vnode = lens _.vnode _ { vnode = _ }

--------------------------------------------------------------------------------

-- | Executes an action in a state transformer on the value of a reference.
stateRef
  :: forall eff s a. Ref s
  -> StateT s (Eff (ref :: REF | eff)) a -> Eff (ref :: REF | eff) a
stateRef r go = readRef r >>= runStateT go >>= \(Tuple a t) -> writeRef r t *> pure a

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
