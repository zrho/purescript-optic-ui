module Main where
--------------------------------------------------------------------------------
import           Prelude
import           Data.Lens
import           Data.Maybe          (maybe)
import           Data.Foldable       (mconcat)
import qualified Data.Array          as AR
import           OpticUI
import qualified OpticUI.Markup.HTML as H
--------------------------------------------------------------------------------

task del = withView H.li_ $ mconcat
  [ completed $ checkBox [ H.titleA "Mark as completed" ]
  , name      $ textField [ H.placeholderA "Task description" ]
  , ui $ H.button [ H.titleA "Remove task", H.onClick (const del) ] (text "X")
  ]

todoList = with $ \s h -> let
  deleted i = runHandler h $ s # tasks %~ maybe [] id <<< AR.deleteAt i
  added     = runHandler h $ s
    # tasks %~ AR.cons { name: s.name, completed: false }
    # name  .~ ""
  num = AR.length s.tasks
  numCompleted = AR.length $ AR.filter _.completed s.tasks
  in mconcat
    [ ui $ H.h1_ $ text "ToDo List"
    , tasks $ foreach (task <<< deleted)
    , name  $ textField [ H.placeholderA "New Task" ]
    , ui $ H.button [ H.onClick (const added) ] $ text "Add"
    , ui $ H.p_ $ text (show numCompleted ++ "/" ++ show num ++ " tasks completed.")
    ]

main = animate { tasks: [], name: "" } todoList

completed = lens _.completed (_ { completed = _ })
name      = lens _.name (_ { name = _ })
tasks     = lens _.tasks (_ { tasks = _ })
