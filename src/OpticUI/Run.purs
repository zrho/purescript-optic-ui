module OpticUI.Run where
--------------------------------------------------------------------------------
import           Prelude
import qualified OpticUI.Internal.VirtualDOM as VD
import           OpticUI.Markup
import           OpticUI.Core
import           Control.Monad         (when)
import           Control.Monad.Eff     (Eff ())
import           Control.Monad.Eff.Ref (REF (), newRef, readRef, writeRef)
import           Control.Monad.State   (State (), runState)
import           Control.Monad.State.Class   
                                       (gets, modify)
import           Data.Function         (Fn2 (), runFn2)
import           Data.Exists           (runExists)
import           Data.Maybe            (Maybe (..))
import           Data.Either           (Either (..))
import           Data.Nullable         (toNullable, toMaybe)
import           Data.Monoid           (Monoid, mempty)
import           Data.Tuple            (Tuple (..))
import           Data.Traversable      (traverse)
import           Data.Array            (foldM)
import           Data.StrMap           (StrMap (), empty, lookup, insert)
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

type MemoInitFin = {initializers :: StrMap VD.Props, finalizers :: StrMap VD.Props}
type Driver eff s = s -> Eff eff Unit

animate
  :: forall s eff. s
  -> UI (dom :: DOM, ref :: REF | eff) Markup s s
  -> Eff (dom :: DOM, ref :: REF | eff) (Driver (dom :: DOM, ref :: REF | eff) s)
animate s0 ui = do
  let v0 = VD.vtext ""
  let n0 = VD.createElement v0
  vR <- newRef v0
  nR <- newRef n0
  gR <- newRef 0
  mR <- newRef ({initializers: empty, finalizers: empty} :: MemoInitFin)
  let
    checkGen g go = do
      h <- readRef gR
      when (g == h) $ do
        writeRef gR (h + 1)
        go
    step gen s = checkGen gen $ do
      v <- readRef vR
      memo <- readRef mR
      (Tuple w newmemo) <- (\tree -> buildVTree tree memo) <$> runUI ui s (Handler $ step $ gen + 1)
      _ <- writeRef mR newmemo
      _ <- writeRef vR w
      n <- readRef nR
      m <- VD.patch (VD.diff v w) n
      writeRef nR m
    driver s = do
      h <- readRef gR
      step h s
  onLoad $ do
    appendToBody n0
    step 0 s0
  return driver

--------------------------------------------------------------------------------

buildVTree :: Markup -> MemoInitFin -> Tuple VD.VTree MemoInitFin
buildVTree (Markup xs) memo = 
  case (runState (traverse toVTree xs) memo) of
       (Tuple tree newmemo) -> Tuple (VD.vnode n "div" n mempty tree) newmemo
         where
           n = toNullable Nothing

toVTree :: Node -> State MemoInitFin VD.VTree
toVTree (Text s) = return $ VD.vtext s
toVTree (Element ns tag props (Markup childs)) = do 
  vprops <- foldM (\acc prop -> (append acc) <$> toVProp prop) mempty props
  tree <- traverse toVTree childs
  return $ VD.vnode (toNullable ns) tag (toNullable Nothing) vprops tree

toVProp :: Prop -> State MemoInitFin VD.Props
toVProp (AttrP n v)          = pure $ runFn2 VD.attrProp n v
toVProp (HandlerP n ee)      = pure $ runEventHandler (\f -> runFn2 VD.handlerProp n f) ee
toVProp (PropP n ee)         = pure $ runExists (\(PropE e) -> runFn2 VD.prop n e) ee
toVProp (InitializerP key f) = findProp _.initializers (\is -> _ {initializers = is}) key $ 
                                 runInitializer (\i -> runFn2 VD.initializer key i) f
toVProp (FinalizerP key f)   = findProp _.finalizers (\fs -> _ {finalizers = fs}) key $ 
                                 runFinalizer (\i -> runFn2 VD.finalizer key i) f

-- Looks for an initializer/finalizer with the same key. If it exists, it uses that one to avoid
-- Virtual Dom removing and adding the node. If it doesn't exist, this function adds it to the 
-- correct StringMap in the state with the unique key
findProp:: (MemoInitFin -> StrMap VD.Props) -> 
           (StrMap VD.Props -> MemoInitFin -> MemoInitFin) -> 
           UniqueStr -> 
           VD.Props -> 
           State MemoInitFin VD.Props
findProp getter setter key newprop = do
  oldprops <- gets getter
  case lookup key oldprops of 
       (Nothing) -> do
         modify $ setter $ insert key newprop oldprops
         return newprop
       (Just oldprop) -> return oldprop

--------------------------------------------------------------------------------

onLoad :: forall eff. Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit
onLoad go = do
  w <- window
  let et = windowToEventTarget w
  addEventListener load (eventListener (\_ -> go)) false et

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = do
  w <- window
  d <- document w
  b <- toMaybe <$> body d
  case b of
    Nothing -> pure unit
    Just b' -> void $ appendChild (htmlElementToNode e) (htmlElementToNode b')
