## Module OpticUI.Components.State

#### `withState`

``` purescript
withState :: forall eff m v s t k. (Monad m) => UI eff (StateT k m) v s t -> UI eff m v (Tuple s k) (Tuple t k)
```


