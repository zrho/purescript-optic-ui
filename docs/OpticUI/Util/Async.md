## Module OpticUI.Util.Async

#### `AsyncT`

``` purescript
type AsyncT eff k = FreeT (AsyncF eff k)
```

#### `async`

``` purescript
async :: forall eff m k. (Monad m) => Aff eff k -> (Error -> Maybe k) -> AsyncT eff k m Unit
```

Run an asynchronous computation that raises a signal with the result.

#### `AsyncF`

``` purescript
data AsyncF eff k a
  = Async (Aff eff k) (Error -> Maybe k) a
```

Async effect.

##### Instances
``` purescript
instance actionFunctor :: Functor (AsyncF eff k)
```

#### `withAsync`

``` purescript
withAsync :: forall eff m k v s t. (MonadEff eff m, MonadRec m) => UI eff (AsyncT eff k m) k v s t -> UI eff m k v s t
```

Run the asynchronous computations a component issues.


