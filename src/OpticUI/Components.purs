module OpticUI.Components
  ( textField, checkBox )
  where
--------------------------------------------------------------------------------
import           Prelude
import           OpticUI.Core
import           OpticUI.Markup
import qualified OpticUI.Markup.HTML as H
import           DOM          (DOM ())
import           Data.Maybe   (maybe)
--------------------------------------------------------------------------------

textField :: forall eff. Array Prop -> UI String (dom :: DOM | eff) Markup
textField attr = do
  v    <- uiState
  inpH <- handler $ \w _ -> pure (maybe "" id w)
  return $ H.input_
    (attr ++ [ H.valueA v, H.typeA "text", H.onInput (const inpH) ])

checkBox :: forall eff. Array Prop -> UI Boolean (dom :: DOM | eff) Markup
checkBox attr = do
  v    <- uiState
  inpH <- handler $ \w _ -> pure w
  return $ H.input_
    (attr ++ [ H.checkedA v, H.typeA "checkbox", H.onChecked (const inpH) ])
