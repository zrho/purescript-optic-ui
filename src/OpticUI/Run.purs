module OpticUI.Run where
--------------------------------------------------------------------------------
import Prelude
import OpticUI.Core
import OpticUI.Component
import OpticUI.Util.Lazy
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Plus
import Control.Apply

type Backend eff k = k (Eff eff Unit) -> Eff eff Unit
type Render eff k = k (Eff eff Unit) -> Eff eff Unit

-- | Animate a `UI` component with a renderer that updates the view of
-- | the component, a backend that manages the component's subscriptions,
-- | and the initial state of the component.
animate
  :: forall eff f s k. (Plus f, Plus k)
  => Render eff f
  -> Backend eff k
  -> s
  -> UI (Eff eff) f k s
  -> Eff eff Unit
animate render backend state ui = case runUI ui state of
  Component subs view -> do
    let step act = act >>= \s -> suspend \_ -> animate render backend s ui
    backend (map step subs)
    render (map step view)

