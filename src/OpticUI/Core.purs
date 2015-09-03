module OpticUI.Core
  ( UI ()
  , runUI
  , zoom
  , zoomOne
  , zoomIxed
  , handler
  , execute
  , inline
  , uiState
  ) where
--------------------------------------------------------------------------------
import           Prelude
import           Optic.Core
import           Optic.Extended
import           OpticUI.Internal.Pretext    (holesOf)
import           OpticUI.Internal.Context    (fromLens)
import           Control.Monad.Eff           (Eff ())
import           Control.Comonad.Store.Class (peek, pos)
import           Data.Monoid                 (Monoid)
import           Data.Foldable               (Foldable, foldr, mconcat)
import           Data.Traversable            (traverse)
import           Data.Tuple                  (Tuple (..))
import           Data.Either                 (Either (..), either)
import           Control.Monad.Trans         (lift)
import qualified Data.List                   as L
import           Control.Monad.Except.Trans  (ExceptT (..), runExceptT, withExceptT)
import           Control.Monad.Error.Class   (throwError)
import           Control.Comonad.Store.Class (ComonadStore, peek, pos)
--------------------------------------------------------------------------------

-- | An UI component with state `s`, effects `eff` and a view result `a`.
newtype UI s eff a = UI
  ( { state :: s, handle :: s -> Eff eff Unit } -> ExceptT (Eff eff s) (Eff eff) a)

instance uiFunctor :: Functor (UI s eff) where
  map f m = m >>= f >>> pure

instance uiApplicative :: Applicative (UI s eff) where
  pure x = UI (const $ pure x)

instance uiApply :: Apply (UI s eff) where
  apply = ap

instance uiBind :: Bind (UI s eff) where
  bind (UI m) f = UI $ \x -> m x >>= \y -> fromUI (f y) x where
    fromUI (UI m) = m

instance uiMonad :: Monad (UI s eff)

runUI
  :: forall s eff a. s -> (s -> Eff eff Unit)
  -> UI s eff a -> Eff eff (Either (Eff eff s) a)
runUI state handle (UI m) = runExceptT $ m { state: state, handle: handle }

--------------------------------------------------------------------------------

-- | Zoom in on a dynamic number of components using a traversal. The zoomed UI
-- | component will be replicated for each target of the traversal in the
-- | encompassing state and will have access to its respective image of the
-- | traversal.
zoom
  :: forall eff s t a. (Monoid a)
  => TraversalP s t -> UI t eff a -> UI s eff a
zoom tr m = uiState >>= holesOf tr >>> traverse (flip zoomStore m) >>> map mconcat

-- | Zoom in one a single component using a lens. The zoomed UI component will
-- | have access to the state restricted to the image of the lens.
zoomOne :: forall eff s t a. LensP s t -> UI t eff a -> UI s eff a
zoomOne l m = uiState >>= fromLens l >>> flip zoomStore m

-- | Zoom in on a dynamic number of components using a traversal, like
-- | [`zoom`](#zoom), keeping track of the index of the zoomed component.
-- |
-- | Note: The time this function was published, the purescript lens library
-- | did not yet support indexed traversals. Eventually, if that support is added,
-- | this function will be altered to take an indexed traversal and pass the
-- | respective index to the zoomed components.
zoomIxed
  :: forall eff s t a. (Monoid a)
  => TraversalP s t -> (Int -> UI t eff a) -> UI s eff a
zoomIxed tr m = do
  hs <- holesOf tr <$> uiState
  as <- L.zipWithA (\i p -> zoomStore p (m i)) (L.range 0 $ L.length hs) hs
  pure (mconcat as)

-- | Access the UI state as seen by this component.
uiState :: forall eff s. UI s eff s
uiState = UI $ \st -> pure st.state

-- | Create a continuation for an event handler function.
handler :: forall eff s e. (e -> s -> Eff eff s) -> UI s eff (e -> Eff eff Unit)
handler h = UI $ \st -> pure $ \e -> h e st.state >>= st.handle

-- | Execute some effectful computation that can change the state. The execution
-- | of the current UI generation is aborted and the UI is rerendered from
-- | scratch.
-- |
-- | Be careful to change the state in a way such that this function is not
-- | triggered again, which would result in an infinite loop.
execute :: forall eff s a. (s -> Eff eff s) -> UI s eff a
execute go = UI $ \st -> throwError (go st.state)

-- | Execute some effectful computation inline with the execution of the current
-- UI generation. The state can not be altered.
-- |
-- | Note that the effect will be executed every time the UI is redrawn, thus
-- | it should not be used to perform visible actions. To perform an action
-- | and change the state, consider using `change`.
inline :: forall eff s a. (s -> Eff eff a) -> UI s eff Unit
inline go = UI $ \st -> const unit <$> lift (go st.state)

--------------------------------------------------------------------------------

-- | Zoom in on the value in a store comonad that would yield the current
-- | state when extracted.
-- |
-- | This function is used to unify the implementation of the other zoom
-- | functions. It is not exported, because using it with a store comonad
-- | that extracts to a different state would yield in strange behaviour.
zoomStore :: forall eff w s t a. (ComonadStore t w) => w s -> UI t eff a -> UI s eff a
zoomStore p (UI m) = UI $ \st -> withExceptT (map $ flip peek p) $ m
  { state: pos p, handle: st.handle <<< flip peek p }
