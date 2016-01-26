## Module OpticUI.Util.State

#### `withState`

``` purescript
withState :: forall eff m k v s t l. (Monad m) => UI eff (StateT l m) k v s t -> UI eff m k v (Tuple s l) (Tuple t l)
```


