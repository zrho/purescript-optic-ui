module Main where
--------------------------------------------------------------------------------
import Prelude
  ( Unit (), unit, pure, show, ($), (#), (<<<), (++), class Applicative)
import Data.Lens
import Data.Lens.Indexed (positions)
import Data.Either (Either (Left), either)
import Data.Maybe (fromMaybe)
import Data.Array as AR
import OpticUI
import OpticUI.Util.Empty (Empty ())
import Web.Markup (Markup (), Prop (..), tag, text)
import Web.Markup.Event (on, on_)
import Web.Markup.IncDOM (renderToBody)
import Web.Markup.HTML as H
import Web.Markup.HTML.Attributes as A
import Web.Markup.HTML.Event as E
import Control.Monad.Eff (Eff ())
import DOM (DOM ())
--------------------------------------------------------------------------------

type Task = { name :: String, completed :: Boolean }
type App  = { name :: String, tasks :: Array Task }

-- | Runs the todo example application using purescript-markup-incdom.
main :: forall eff. Eff (dom :: DOM | eff) Unit
main = animate
  renderToBody
  (\_ -> pure unit)
  { tasks: [], name: "" }
  (effect suspendLazy todoList)

-- | The example todo-list application.
todoList :: UI Lazy Markup Empty App
todoList = withState \s ->
  let
    -- event handlers
    taskAdded = defer \_ -> s
      # tasks %~ AR.cons { name: s.name, completed: false }
      # name  .~ ""
    taskDeleted i = defer \_ -> s
      # tasks %~ fromMaybe [] <<< AR.deleteAt i

    -- counters
    numTasks    = AR.length s.tasks
    numComplete = AR.length $ AR.filter _.completed s.tasks

    -- UI component for task list
    tasksUI
      = overView (H.ul [])
      $ effect (either taskDeleted pure)
      $ positions (tasks <<< traversed)
      $ ixui taskUI
  in
    mconcat
      [ mkView $ H.h1 [] (text "ToDo List")
      , tasksUI
      , name   $ textField [ A.placeholder "New Task", Key "name" ]
      , mkView $ H.button [ on_ E.Click taskAdded ] (text "Add Task")
      , mkView $ H.p [] $ text (show numComplete ++ "/" ++ show numTasks ++ " tasks completed.")
      ]

-- | Component that displays a task. If `Left i` is raised as an event,
-- | the parent component is instructed to delete the associated task.
taskUI :: Int -> UI (Either Int) Markup Empty Task
taskUI i = withState \s -> overView (H.li []) $ mconcat
  [ completed $ checkBox [ A.title "Mark as completed" ]
  , name      $ textField [ A.placeholder "Task description" ]
  , mkView    $ H.button [ A.title "Remove task", on_ E.Click (Left i) ] (text "X")
  ]

textField :: forall m k. (Plus k, Applicative m) => Array (Prop (m String)) -> UI m Markup k String
textField ps = withState \s -> mkView $ H.input (ps ++ [A.value s, on E.Input pure]) mempty

checkBox :: forall m k. (Plus k, Applicative m) => Array (Prop (m Boolean)) -> UI m Markup k Boolean
checkBox ps = withState \s -> mkView $ H.input (ps ++ [A.value (show s), on E.Checked pure, A.type_ "checkbox"]) mempty

completed = lens _.completed (_ { completed = _ })
name      = lens _.name (_ { name = _ })
tasks     = lens _.tasks (_ { tasks = _ })

