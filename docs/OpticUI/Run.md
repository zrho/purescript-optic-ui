## Module OpticUI.Run

#### `AnimateE`

``` purescript
type AnimateE eff = (dom :: DOM, ref :: REF | eff)
```

#### `animate`

``` purescript
animate :: forall s eff k. s -> UI (AnimateE eff) (Eff (AnimateE eff)) k Markup s s -> Eff (AnimateE eff) (Driver eff k)
```


