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
instance remoteFunctor :: Functor (Remote e)
instance remoteApply :: Apply (Remote e)
instance remoteApplicative :: Applicative (Remote e)
instance remoteBind :: Bind (Remote e)
instance remoteMonad :: Monad (Remote e)
instance remoteBifunctor :: Bifunctor Remote
instance remoteSemigroup :: (Semigroup a) => Semigroup (Remote e a)
instance remoteMonoid :: (Monoid a) => Monoid (Remote e a)
instance remoteFoldable :: Foldable (Remote e)
instance remoteTraversable :: Traversable (Remote e)
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


