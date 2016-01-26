module OpticUI.Components
  ( checkBox
  , textField
  ) where
--------------------------------------------------------------------------------
import           Prelude
import           OpticUI.Core
import           OpticUI.Markup
import qualified OpticUI.Markup.HTML as H
import           Data.Maybe   (maybe)
--------------------------------------------------------------------------------

textField :: forall eff m k. (Applicative m) => Array Prop -> UI eff m k Markup String String
textField as = with $ \s h -> let
  inpH _ = updatePure h <<< maybe "" id
  bs     = [ H.valueA s, H.typeA "text", H.onInput inpH ]
  in ui $ H.input_ (as ++ bs)

checkBox :: forall eff m k. (Applicative m) => Array Prop -> UI eff m k Markup Boolean Boolean
checkBox as = with $ \s h -> let
  inpH _ = updatePure h
  bs     = [ H.checkedA s, H.typeA "checkbox", H.onChecked inpH ]
  in ui $ H.input_ (as ++ bs)
