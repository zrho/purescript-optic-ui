module OpticUI.Run (animate) where
--------------------------------------------------------------------------------
import           Prelude
import qualified OpticUI.Internal.VirtualDOM as VD
import           OpticUI.HTML
import           OpticUI.Core
import           Control.Monad         (when)
import           Control.Monad.Eff     (Eff ())
import           Control.Monad.Eff.Ref (REF (), newRef, readRef, writeRef)
import           Data.Function         (Fn2 (), runFn2)
import           Data.Exists           (runExists)
import           Data.Maybe            (Maybe (..))
import           Data.Either           (Either (..))
import           Data.Nullable         (toNullable, toMaybe)
import           Data.Tuple            (Tuple (..))
import           Data.Foldable         (foldMap)
import           DOM                   (DOM ())
import           DOM.Event.EventTarget (eventListener, addEventListener)
import           DOM.Event.EventTypes  (load)
import           DOM.HTML              (window)
import           DOM.HTML.Document     (body)
import           DOM.Node.Element      (getAttribute)
import           DOM.HTML.Types
  ( HTMLElement (), htmlElementToNode, windowToEventTarget)
import           DOM.HTML.Window       (document)
import           DOM.Node.Node         (appendChild)
--------------------------------------------------------------------------------

animate
  :: forall s eff. s
  -> UI s (dom :: DOM, ref :: REF | eff) (HTML (dom :: DOM, ref :: REF | eff))
  -> Eff (dom :: DOM, ref :: REF | eff) Unit
animate s0 ui = do
  let v0 = VD.vtext ""
  let n0 = VD.createElement v0
  vR <- newRef v0
  nR <- newRef n0
  gR <- newRef 0
  let
    checkGen g go = do
      h <- readRef gR
      when (g == h) $ do
        writeRef gR (h + 1)
        go
    step gen s = checkGen gen $ do
      v <- readRef vR
      r <- runUI s (step $ gen + 1) ui
      case r of
        Right h -> do
          let w = toVTree h
          _ <- writeRef vR w
          n <- readRef nR
          m <- VD.patch (VD.diff v w) n
          writeRef nR m
        Left go -> go >>= step (gen + 1)
  onLoad $ do
    appendToBody n0
    step 0 s0

--------------------------------------------------------------------------------

toVTree :: forall eff. HTML eff -> VD.VTree
toVTree (Text s) = VD.vtext s
toVTree (Element ns tag props childs) = VD.vnode
  (toNullable ns) tag (toNullable Nothing)
  (foldMap toVProp props) (map toVTree childs)

toVProp :: forall eff. Prop eff -> VD.Props
toVProp (Attr n v)   = runFn2 VD.attrProp n v
toVProp (Handler ee) = runExists (\(HandlerE n e) -> runFn2 VD.handlerProp n e) ee
toVProp (Prop ee)    = runExists (\(PropE n e) -> runFn2 VD.prop n e) ee

--------------------------------------------------------------------------------

onLoad :: forall eff. Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit
onLoad go = do
  w <- window
  let et = windowToEventTarget w
  addEventListener load (eventListener (\_ -> go)) false et

appendToBody :: forall m eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = do
  w <- window
  d <- document w
  b <- toMaybe <$> body d
  case b of
    Nothing -> pure unit
    Just b' -> void $ appendChild (htmlElementToNode e) (htmlElementToNode b')
