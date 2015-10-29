module OpticUI.Components
  ( checkBox
  , textField
  ) where
--------------------------------------------------------------------------------
import           Prelude
import           OpticUI.Core
import           OpticUI.Markup
import qualified OpticUI.Markup.HTML as H
import           DOM          (DOM ())
import           Data.Maybe   (maybe)
--------------------------------------------------------------------------------

textField :: forall eff. Array Prop -> UI (dom :: DOM | eff) Markup String String
textField as = with $ \s h -> let
  inpH _ = runHandler h <<< maybe "" id
  bs     = [ H.valueA s, H.typeA "text", H.onInput inpH ]
  in ui $ H.input_ (as ++ bs)

checkBox :: forall eff. Array Prop -> UI (dom :: DOM | eff) Markup Boolean Boolean
checkBox as = with $ \s h -> let
  inpH _ = runHandler h
  bs     = [ H.checkedA s, H.typeA "checkbox", H.onChecked inpH ]
  in ui $ H.input_ (as ++ bs)
