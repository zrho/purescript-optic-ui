module Main where
--------------------------------------------------------------------------------
import           Prelude
import           Optic.Core
import           Optic.Extended
import           Data.Maybe              (maybe)
import           Data.Traversable        (traverse, sequence)
import qualified Data.List               as L
import qualified Data.Array              as AR
import           OpticUI
import qualified OpticUI.HTML.Elements   as H
import qualified OpticUI.HTML.Attributes as A
import qualified OpticUI.HTML.Handlers   as H
--------------------------------------------------------------------------------

task del = H.li_ <$> sequence
  [ zoomOne completed $ checkBox [ A.title "Mark as completed" ]
  , zoomOne name      $ textField [ A.placeholder "Task description" ]
  , pure $ H.button [ A.title "Remove task", H.onClick (const del) ] [ H.text "X" ]
  ]

todoList = do
  delH <- handler $ \i s -> pure $ s
    # tasks %~ maybe [] id <<< AR.deleteAt i
  addH <- handler $ \_ s -> pure $ s
    # tasks %~ AR.cons { name : s ^. name, completed : false }
    # name  .~ ""
  num <- AR.length <<< _.tasks <$> uiState
  numCompleted <- AR.length <<< AR.filter _.completed <<< _.tasks <$> uiState
  H.div_ <$> sequence
    [ pure $ H.h1_ [ H.text "ToDo List "]
    , H.ul_ <<< L.fromList <$> zoomIxed (tasks <<< traverse) (task <<< delH)
    , zoomOne name $ textField [ A.placeholder "New Task" ]
    , pure $ H.button [ H.onClick addH ] [ H.text "Add" ]
    , pure $ H.p_
      [ H.text (show numCompleted ++ "/" ++ show num ++ " tasks completed.") ]
    ]

main = animate { name: "", tasks: [] } $ todoList

completed = lens (_.completed) (\r v -> r { completed = v })
name = lens (_.name) (\r v -> r { name = v })
tasks = lens (_.tasks) (\r v -> r { tasks = v })
