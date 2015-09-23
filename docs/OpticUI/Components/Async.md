## Module OpticUI.Components.Async

#### `Async`

``` purescript
data Async eff a
  = Async (Ref (Either Error a -> Eff eff Unit))
```

#### `onResult`

``` purescript
onResult :: forall eff v t a. (Monoid v) => (a -> Eff (ref :: REF | eff) Unit) -> (Error -> Eff (ref :: REF | eff) Unit) -> UI (ref :: REF | eff) v (Async (ref :: REF | eff) a) t
```

#### `async`

``` purescript
async :: forall eff a. Aff (ref :: REF | eff) a -> Eff (ref :: REF | eff) (Async (ref :: REF | eff) a)
```


