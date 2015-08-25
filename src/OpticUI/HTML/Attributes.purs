module OpticUI.HTML.Attributes where
--------------------------------------------------------------------------------
import Prelude
import OpticUI.HTML (attr, prop, Prop (..))
--------------------------------------------------------------------------------

text :: forall eff. String -> Prop eff
text = attr "text"

title :: forall eff. String -> Prop eff
title = attr "title"

type_ :: forall eff. String -> Prop eff
type_ = attr "type"

placeholder :: forall eff. String -> Prop eff
placeholder = attr "placeholder"

value :: forall eff. String -> Prop eff
value = prop "value"

checked :: forall eff. Boolean -> Prop eff
checked = prop "checked"
