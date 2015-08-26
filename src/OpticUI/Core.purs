module OpticUI.Core
  (UI (..), zoomOne, zoomAll, zoomIxed, handler, uiState)
  where
--------------------------------------------------------------------------------
import           Prelude
import           Optic.Core
import           Optic.Extended
import           OpticUI.Internal.Pretext    (holesOf)
import           Control.Monad.Eff           (Eff ())
import           Control.Comonad.Store.Class (peek, pos)
import           Data.Traversable            (traverse)
import           Data.Tuple                  (Tuple (..))
import qualified Data.List as L
--------------------------------------------------------------------------------

-- | An UI component with state `s`, effects `eff` and a view result `a`.
newtype UI s eff a = UI (Tuple s (s -> Eff eff Unit) -> Eff eff a)

instance uiFunctor :: Functor (UI s eff) where
  map f m = m >>= f >>> pure

instance uiApplicative :: Applicative (UI s eff) where
  pure x = UI (const $ pure x)

instance uiApply :: Apply (UI s eff) where
  apply ff fx = do
    f <- ff
    x <- fx
    return (f x)

instance uiBind :: Bind (UI s eff) where
  bind (UI m) f = UI $ \x -> m x >>= \y -> fromUI (f y) x where
    fromUI (UI m) = m

instance uiMonad :: Monad (UI s eff)

--------------------------------------------------------------------------------

-- | Zoom in one a single component using a lens. The zoomed UI component will
-- | have access to the state restricted to the image of the lens.
zoomOne :: forall eff s t a. LensP s t -> UI t eff a -> UI s eff a
zoomOne l (UI m) = UI $ \(Tuple s fs) -> m $ Tuple (s ^. l) (fs <<< flip (set l) s)

-- | Zoom in on a dynamic number of components using a traversal. The zoomed UI
-- | component will be replicated for each target of the traversal in the
-- | encompassing state and will have access to its respective image of the
-- | traversal.
zoomAll :: forall eff s t a. TraversalP s t -> UI t eff a -> UI s eff (L.List a)
zoomAll tr (UI m) = UI $ \(Tuple s fs) -> let
  hs   = holesOf tr s
  go p = m $ Tuple (pos p) (fs <<< flip peek p)
  in traverse go hs

-- | Zoom in on a dynamic number of components using a traversal, like
-- | [`zoomAll`](#zoomAll), keeping track of the index of the zoomed component.
-- |
-- | Note: The time this function was published, the purescript lens library
-- | did not yet support indexed traversals. Eventually, if that support is added,
-- | this function will be altered to take an indexed traversal and pass the
-- | respective index to the zoomed components.
zoomIxed :: forall eff s t a. TraversalP s t -> (Int -> UI t eff a) -> UI s eff (L.List a)
zoomIxed tr m = UI $ \(Tuple s fs) -> let
  hs             = holesOf tr s
  go (Tuple i p) = fromUI (m i) $ Tuple (pos p) (fs <<< flip peek p)
  fromUI (UI t)  = t
  in traverse go (L.zip (0 L... L.length hs) hs)

-- | Create a continuation for an event handler function.
handler :: forall eff s e. (e -> s -> Eff eff s) -> UI s eff (e -> Eff eff Unit)
handler h = UI $ \(Tuple s fs) -> pure $ \e -> h e s >>= fs

-- | Access the UI state as seen by this component.
uiState :: forall eff s. UI s eff s
uiState = UI $ \(Tuple s _) -> pure s
