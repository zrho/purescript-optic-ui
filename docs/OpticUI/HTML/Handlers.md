## Module OpticUI.HTML.Handlers

#### `onClick`

``` purescript
onClick :: forall eff. (Event () -> Eff eff Unit) -> Prop eff
```

#### `onInput`

``` purescript
onInput :: forall eff a. (IsForeign a) => (Event () -> Maybe a -> Eff eff Unit) -> Prop eff
```

#### `onChecked`

``` purescript
onChecked :: forall eff a. (Event () -> Boolean -> Eff eff Unit) -> Prop eff
```

#### `getProp`

``` purescript
getProp :: forall a r. (IsForeign a) => String -> Event r -> Maybe a
```


