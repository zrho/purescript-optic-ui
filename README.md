Optic UI
================

[![Build Status](https://travis-ci.org/zrho/purescript-optic-ui.svg?branch=master)](https://travis-ci.org/zrho/purescript-optic-ui)
[![Maintainer: zrho](https://img.shields.io/badge/maintainer-zrho-lightgrey.svg)](http://github.com/zrho)

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
  let clicked _ = updatePure h (not on)
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

For more examples, have a look at the `examples` directory:

- [Todo Manager](examples/todo/src/Main.purs)
- [AJAX Example](examples/ajax/src/Main.purs)
- [Effects Example](example/effects/src/Main.purs)
