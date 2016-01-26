module OpticUI.Util.State (withState) where
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
  :: forall eff m k v s t l. (Monad m)
  => UI eff (StateT l m) k v s t
  -> UI eff m k v (Tuple s l) (Tuple t l)
withState ui = with \(Tuple _ k) _ -> withEffects (flip runStateT k) (lmap fst ui)
