module Main where
--------------------------------------------------------------------------------
import           Prelude
import           Optic.Core
import           Optic.Extended
import           Data.Maybe              (maybe)
import           Data.Foldable           (mconcat)
import           Data.Traversable        (traverse, sequence)
import qualified Data.List               as L
import qualified Data.Array              as AR
import           OpticUI
import           OpticUI.Util.Writer
import qualified OpticUI.Markup.HTML as H
--------------------------------------------------------------------------------

task del = collect H.li_ $ do
  zoomW completed $ checkBox [ H.titleA "Mark as completed" ]
  zoomW name      $ textField [ H.placeholderA "Task description" ]
  tell $ H.button [ H.titleA "Remove task", H.onClick (const del) ] (text "X")

todoList = do
  delH <- handler $ \i s -> pure $ s
    # tasks %~ maybe [] id <<< AR.deleteAt i
  addH <- handler $ \_ s -> pure $ s
    # tasks %~ AR.cons { name : s ^. name, completed : false }
    # name  .~ ""
  num <- AR.length <<< _.tasks <$> uiState
  numCompleted <- AR.length <<< AR.filter _.completed <<< _.tasks <$> uiState
  collect H.div_ $ do
    tell $ H.h1_ $ text "ToDo List"
    lift (zoomIxed (tasks <<< traverse) (task <<< delH)) >>= H.ul_ >>> tell
    zoomW name $ textField [ H.placeholderA "New Task" ]
    tell $ H.button [ H.onClick addH ] $ text "Add"
    tell $ H.p_ $ text (show numCompleted ++ "/" ++ show num ++ " tasks completed.")

main = animate { name: "", tasks: [] } $ todoList

completed = lens (_.completed) (\r v -> r { completed = v })
name = lens (_.name) (\r v -> r { name = v })
tasks = lens (_.tasks) (\r v -> r { tasks = v })
