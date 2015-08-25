module OpticUI.Components
  ( textField, checkBox )
  where
--------------------------------------------------------------------------------
import           Prelude
import           OpticUI.Core
import           OpticUI.HTML
import           DOM          (DOM ())
import           Data.Maybe   (maybe)
import qualified OpticUI.HTML.Elements   as H
import qualified OpticUI.HTML.Attributes as A
import qualified OpticUI.HTML.Handlers   as H
--------------------------------------------------------------------------------

textField
  :: forall eff. Array (Prop (dom :: DOM | eff))
  -> UI String (dom :: DOM | eff) (HTML (dom :: DOM | eff))
textField attr = do
  v    <- uiState
  inpH <- handler $ \w _ -> pure (maybe "" id w)
  return $ H.input_
    (attr ++ [ A.value v, A.type_ "text", H.onInput (const inpH) ])

checkBox
  :: forall eff. Array (Prop (dom :: DOM | eff))
  -> UI Boolean (dom :: DOM | eff) (HTML (dom :: DOM | eff))
checkBox attr = do
  v    <- uiState
  inpH <- handler $ \w _ -> pure w
  return $ H.input_
    (attr ++ [ A.checked v, A.type_ "checkbox", H.onChecked (const inpH) ])
