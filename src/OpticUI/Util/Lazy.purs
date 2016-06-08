module OpticUI.Util.Lazy where
--------------------------------------------------------------------------------
import Prelude
import Data.Lazy
import Control.Monad.Eff

-- | Suspends evaluation of an effect until it is run.
foreign import suspend :: forall eff a. (Unit -> Eff eff a) -> Eff eff a

-- | Converts a lazy value to an effect that forces on execution.
suspendLazy :: forall eff a. Lazy a -> Eff eff a
suspendLazy t = suspend \_ -> pure (force t)
