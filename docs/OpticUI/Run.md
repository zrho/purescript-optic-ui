## Module OpticUI.Run

#### `AnimateE`

``` purescript
type AnimateE eff = (dom :: DOM | eff)
```

#### `animate`

``` purescript
animate :: forall s eff. s -> UI (AnimateE eff) (Eff (AnimateE eff)) Markup s s -> Eff (AnimateE eff) Unit
```


