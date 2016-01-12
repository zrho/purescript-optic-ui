## Module OpticUI.Components.Async

#### `Async`

``` purescript
data Async eff a
  = Async (Ref (Either Error a -> Eff eff Unit))
```

#### `onResult`

``` purescript
onResult :: forall eff m v t a. (Monoid v, Functor m) => (a -> Eff (ref :: REF | eff) Unit) -> (Error -> Eff (ref :: REF | eff) Unit) -> UI (ref :: REF | eff) m v (Async (ref :: REF | eff) a) t
```

#### `async`

``` purescript
async :: forall eff a. Aff (ref :: REF | eff) a -> Eff (ref :: REF | eff) (Async (ref :: REF | eff) a)
```


