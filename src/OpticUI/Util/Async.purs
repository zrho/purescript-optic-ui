module OpticUI.Util.Async where
--------------------------------------------------------------------------------
import Prelude
import OpticUI.Core
import Control.Monad.Free.Trans
import Control.Monad.State.Trans
import Control.Monad.Eff.Class
import Control.Monad.Rec.Class
import Control.Monad.Eff.Exception (Error ())
import Control.Monad.Aff
import Control.Apply
import Data.Maybe
import Data.Foldable
--------------------------------------------------------------------------------

-- | Monad transformer for conveniently executing asynchronous actions.
type AsyncT eff k = FreeT (AsyncF eff k)

-- | Run an asynchronous computation that raises a signal with the result.
async :: forall eff m k. (Monad m) => Aff eff k -> (Error -> Maybe k) -> AsyncT eff k m Unit
async go err = liftFreeT (Async go err unit)

-- | Async effect.
data AsyncF eff k a = Async (Aff eff k) (Error -> Maybe k) a

instance actionFunctor :: Functor (AsyncF eff k) where
  map f (Async go err r) = Async go err (f r)

-- | Run the asynchronous computations a component issues.
withAsync
  :: forall eff m k v s t. (MonadEff eff m, MonadRec m)
  => UI eff (AsyncT eff k m) k v s t -> UI eff m k v s t
withAsync ui = with \_ h -> let
  go (Async aff err a) = liftEff do
    runAff (maybe (pure unit) (raise h) <<< err) (raise h) aff
    pure a
  in withEffects (runFreeT go) ui
