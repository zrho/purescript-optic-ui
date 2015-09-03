Optic UI
================

Optic UI is a Purescript library that allows to write single page web user
interfaces declaratively and concisely with the help of lenses and traversals:

UI components are defined in a monad that gives them access to a view model, a
state local to the component. Child components that access only a part of the
state of their parent can be embedded in a bigger component using lenses and
traversals that focus on the respective sub-states. Handler functions that
manipulate the local state of a component can be transformed into continuations
that update the entire UI. These continuations are then attached to a
component's view, the result of the monad, which is displayed efficiently using
virtual-dom.

Examples
---------------------

```purescript
main = animate false $ do
  click <- handler $ \_ s -> pure (not s)
  on    <- uiState
  pure $ H.button
    [ H.titleA "Toggle", H.onClick h ]
    [ H.text $ if on then "On" else "Off" ]
```

```purescript
main = animate "Hello World" $ H.div_ <$> sequence
  [ H.p_ [ H.text "First text field:" ]
  , zoomOne id $ textField []
  , H.p_ [ H.text "Second text field:" ]
  , zoomOne id $ textField []
  ]
```

```purescript
_1 = lens (\(Tuple a _) -> a) (\(Tuple _ b) a -> Tuple a b)
_2 = lens (\(Tuple _ b) -> b) (\(Tuple a _) b -> Tuple a b)

main = animate (Tuple "Left" "Right") $ H.div_ <$> sequence
  [ zoomOne _1 $ textField []
  , zoomOne _2 $ textField []
  ]
```
