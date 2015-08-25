module OpticUI.HTML.Handlers where
--------------------------------------------------------------------------------
import Prelude
import OpticUI.HTML (Prop (..), handle, Event (..))
import Data.Foreign (toForeign)
import Data.Foreign.Class (IsForeign, readProp)
import Data.Maybe (Maybe (..), maybe)
import Data.Either (either)
import Control.Monad.Eff (Eff())
--------------------------------------------------------------------------------

onClick :: forall eff. (Event () -> Eff eff Unit) -> Prop eff
onClick h = handle "click" h

onInput :: forall eff a. (IsForeign a) => (Event () -> Maybe a -> Eff eff Unit) -> Prop eff
onInput h = handle "input" $ \e -> h e (getProp "value" e)

onChecked :: forall eff a. (Event () -> Boolean -> Eff eff Unit) -> Prop eff
onChecked h = handle "change" $ \e -> h e (maybe false id $ getProp "checked" e)

--------------------------------------------------------------------------------

getProp :: forall a r. (IsForeign a) => String -> Event r -> Maybe a
getProp p = either (const Nothing) Just <<< readProp p <<< toForeign <<< _.target
