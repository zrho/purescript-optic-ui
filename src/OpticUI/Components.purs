module OpticUI.Components
  ( checkBox
  , textField
  ) where
--------------------------------------------------------------------------------
import Prelude             (class Applicative, (++), ($), id, (<<<))
import Data.Maybe          (maybe)
import OpticUI.Core        (UI, ui, updatePure, with)
import OpticUI.Markup      (Markup, Prop)
import OpticUI.Markup.HTML as H
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
