## Module OpticUI.Action

#### `Action`

``` purescript
type Action eff k = FreeT (ActionF eff k) (Eff eff)
```

#### `async`

``` purescript
async :: forall eff k. Aff eff k -> (Error -> Maybe k) -> Action eff k Unit
```

Run an asynchronous computation and raise a signal with the result.

#### `ActionF`

``` purescript
data ActionF eff k a
  = Async (Aff eff k) (Error -> Maybe k) a
```

##### Instances
``` purescript
instance actionFunctor :: Functor (ActionF eff k)
```


