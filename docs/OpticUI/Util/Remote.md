## Module OpticUI.Util.Remote

#### `Remote`

``` purescript
data Remote e a
  = Success a
  | Failed e
  | Pending
  | Init
```

##### Instances
``` purescript
Functor (Remote e)
Apply (Remote e)
Applicative (Remote e)
Bind (Remote e)
Monad (Remote e)
Bifunctor Remote
(Semigroup a) => Semigroup (Remote e a)
(Monoid a) => Monoid (Remote e a)
Foldable (Remote e)
Traversable (Remote e)
```

#### `toRemote`

``` purescript
toRemote :: forall e a. Either e a -> Remote e a
```

#### `_Success`

``` purescript
_Success :: forall e a b. Prism (Remote e a) (Remote e b) a b
```

#### `_Failed`

``` purescript
_Failed :: forall e t a. Prism (Remote e a) (Remote t a) e t
```

#### `_Pending`

``` purescript
_Pending :: forall e a. PrismP (Remote e a) Unit
```

#### `_Init`

``` purescript
_Init :: forall e a. PrismP (Remote e a) Unit
```


