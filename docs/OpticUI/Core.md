## Module OpticUI.Core

#### `UI`

``` purescript
newtype UI eff m k v s t
```

##### Instances
``` purescript
instance uiProfunctor :: (Functor m) => Profunctor (UI eff m k v)
instance uiChoice :: (Monoid v, Functor m) => Choice (UI eff m k v)
instance uiStrong :: (Functor m) => Strong (UI eff m k v)
instance uiSemigroup :: (Semigroup v) => Semigroup (UI eff m k v s t)
instance uiMonoid :: (Monoid v) => Monoid (UI eff m k v s t)
```

#### `UI_`

``` purescript
type UI_ eff m k v s = UI eff m k v s s
```

Shorthand for user interfaces with the same input and output state.

#### `runUI`

``` purescript
runUI :: forall eff m k v s t. UI eff m k v s t -> s -> Handler eff m k t -> Result eff k v
```

#### `Result`

``` purescript
data Result eff k v
  = Result (k -> Eff eff Boolean) v
```

##### Instances
``` purescript
instance resultSemigroup :: (Semigroup v) => Semigroup (Result eff k v)
instance resultMonoid :: (Monoid v) => Monoid (Result eff k v)
```

#### `Handler`

``` purescript
data Handler eff m k s
  = Handler (m s -> Eff eff Unit) (k -> Eff eff Unit)
```

##### Instances
``` purescript
instance handlerContravariant :: (Functor m) => Contravariant (Handler eff m k)
```

#### `update`

``` purescript
update :: forall eff m k s. Handler eff m k s -> m s -> Eff eff Unit
```

#### `updatePure`

``` purescript
updatePure :: forall eff m k s. (Applicative m) => Handler eff m k s -> s -> Eff eff Unit
```

#### `raise`

``` purescript
raise :: forall eff m k s. Handler eff m k s -> k -> Eff eff Unit
```

#### `ui`

``` purescript
ui :: forall eff m k v s t. v -> UI eff m k v s t
```

#### `with`

``` purescript
with :: forall eff m k v s t. (s -> Handler eff m k t -> UI eff m k v s t) -> UI eff m k v s t
```

Access the state and the handler for an `UI` component.

#### `withView`

``` purescript
withView :: forall eff m k v w s t. (v -> w) -> UI eff m k v s t -> UI eff m k w s t
```

Manipulate the view of an `UI` component.

#### `withEffects`

``` purescript
withEffects :: forall eff m w k v s t l. (m t -> w l) -> UI eff m k v s t -> UI eff w k v s l
```

Interpret the effects of an `UI` component in a different monad.

#### `withSignal`

``` purescript
withSignal :: forall eff m k l v s t. APrismP l k -> UI eff m k v s t -> UI eff m l v s t
```

Translate and route signals.

#### `listen`

``` purescript
listen :: forall eff m k v s t. (Monoid v) => (k -> Boolean) -> (k -> Eff eff Unit) -> UI eff m k v s t
```

Listen to a signal matching a predicate and execute an action on match.

#### `traversal`

``` purescript
traversal :: forall eff m k v s t. (Monoid v, Functor m) => Traversal s s t t -> UI eff m k v t t -> UI eff m k v s s
```

Display a `UI` component for each focus of a `Traversal`.

#### `foreach`

``` purescript
foreach :: forall eff m k v s t. (Monoid v, Traversable t, Functor m) => (Int -> UI eff m k v s s) -> UI eff m k v (t s) (t s)
```

Display a `UI` component for each element of a `Traversable` container,
with access to the index into the container.


