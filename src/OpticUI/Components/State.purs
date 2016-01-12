module OpticUI.Components.State (withState) where
--------------------------------------------------------------------------------
import Prelude
import Control.Monad.State.Trans (StateT (), runStateT)
import Data.Tuple (Tuple (..), fst)
import Data.Profunctor (lmap)
import OpticUI.Core
--------------------------------------------------------------------------------

-- | Manage the state of an `UI` component with effects in a state monad
-- | transformer in a part of the `UI` state.
withState
  :: forall eff m v s t k. (Monad m)
  => UI eff (StateT k m) v s t
  -> UI eff m v (Tuple s k) (Tuple t k)
withState ui = with \(Tuple _ k) _ -> withEffects (flip runStateT k) (lmap fst ui)
