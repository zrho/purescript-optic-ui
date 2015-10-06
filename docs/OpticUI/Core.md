## Module OpticUI.Core

#### `UI`

``` purescript
newtype UI eff v s t
```

##### Instances
``` purescript
instance uiProfunctor :: Profunctor (UI eff v)
instance uiChoice :: (Monoid v) => Choice (UI eff v)
instance uiStrong :: Strong (UI eff v)
instance uiSemigroup :: (Semigroup v) => Semigroup (UI eff v s t)
instance uiMonoid :: (Monoid v) => Monoid (UI eff v s t)
```

#### `runUI`

``` purescript
runUI :: forall eff v s t. UI eff v s t -> s -> Handler eff t -> Eff eff v
```

#### `Handler`

``` purescript
newtype Handler eff s
  = Handler (s -> Eff eff Unit)
```

##### Instances
``` purescript
instance handlerContravariant :: Contravariant (Handler eff)
```

#### `runHandler`

``` purescript
runHandler :: forall eff s. Handler eff s -> s -> Eff eff Unit
```

#### `ui`

``` purescript
ui :: forall eff v s t. v -> UI eff v s t
```

#### `with`

``` purescript
with :: forall eff v s t. (s -> Handler eff t -> UI eff v s t) -> UI eff v s t
```

Access the state and the handler for an `UI` component.

#### `withView`

``` purescript
withView :: forall eff v w s t. (v -> w) -> UI eff v s t -> UI eff w s t
```

Manipulate the view of an `UI` component.

#### `traversal`

``` purescript
traversal :: forall eff v s t. (Monoid v) => Traversal s s t t -> UI eff v t t -> UI eff v s s
```

Display a `UI` component for each focus of a `Traversal`.

#### `foreach`

``` purescript
foreach :: forall eff v s t. (Monoid v, Traversable t) => (Int -> UI eff v s s) -> UI eff v (t s) (t s)
```

Display a `UI` component for each element of a `Traversable` container,
with access to the index into the container.

#### `inline`

``` purescript
inline :: forall eff v s t a. Eff eff v -> UI eff v s t
```

Create a `UI` component that executes an action while build.


