Optic UI
================

Optic UI is a Purescript library that allows to write single page web user
interfaces declaratively and concisely with the help of lenses and traversals:

UI components are defined in a profunctor that gives them access to a view
model, a state local to the component. Child components that access only a part
of the state of their parent can be embedded in a bigger component using lenses
and traversals that focus on the respective sub-states. Components provide a
handler function that - given a new state - triggers an update of the UI and
generate a view that is finally rendered using virtual-dom.

Examples
---------------------

```purescript
main = animate false $ with \on h ->
  let clicked _ = runHandler h (not on)
  in mconcat $ ui <$>
    [ H.h1_ $ text "Toggle Button"
    , H.button [H.titleA "Toggle", H.onClick clicked] $ text $ if on then "On" else "Off"
    ]
```

```purescript
main = animate "Hello World" $ withView H.div_ $ with \s h -> mconcat
  [ ui $ H.h1_ $ text "Synchronized Text Fields"
  , ui $ H.p_ $ text "First text field:"
  , textField []
  , ui $ H.p_ $ text "Second text field:"
  , textField []
  ]
```

```purescript
main = animate (Tuple "Left" "Right") $ withView H.div_ $ mconcat
  [ ui $ H.h1_ $ text "Independent Text Fields"
  , ui $ H.p_ $ text "First text field:"
  , _1 $ textField []
  , ui $ H.p_ $ text "Second text field:"
  , _2 $ textField []
  ]
```

For more examples, have a look at the `examples` directory.
