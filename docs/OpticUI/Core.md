## Module OpticUI.Core

#### `UI`

``` purescript
newtype UI eff m v s t
```

##### Instances
``` purescript
(Functor m) => Profunctor (UI eff m v)
(Monoid v, Functor m) => Choice (UI eff m v)
(Functor m) => Strong (UI eff m v)
(Semigroup v) => Semigroup (UI eff m v s t)
(Monoid v) => Monoid (UI eff m v s t)
```

#### `UI_`

``` purescript
type UI_ eff m v s = UI eff m v s s
```

#### `runUI`

``` purescript
runUI :: forall eff v s t. UI eff (Eff eff) v s t -> s -> Handler eff (Eff eff) t -> Eff eff v
```

#### `Handler`

``` purescript
newtype Handler eff m s
  = Handler (m s -> Eff eff Unit)
```

##### Instances
``` purescript
(Functor m) => Contravariant (Handler eff m)
```

#### `update`

``` purescript
update :: forall eff m s. Handler eff m s -> m s -> Eff eff Unit
```

#### `updatePure`

``` purescript
updatePure :: forall eff m s. (Applicative m) => Handler eff m s -> s -> Eff eff Unit
```

#### `ui`

``` purescript
ui :: forall eff m v s t. v -> UI eff m v s t
```

#### `with`

``` purescript
with :: forall eff m v s t. (s -> Handler eff m t -> UI eff m v s t) -> UI eff m v s t
```

Access the state and the handler for an `UI` component.

#### `withView`

``` purescript
withView :: forall eff m v w s t. (v -> w) -> UI eff m v s t -> UI eff m w s t
```

Manipulate the view of an `UI` component.

#### `withEffects`

``` purescript
withEffects :: forall eff m w v s t k. (m t -> w k) -> UI eff m v s t -> UI eff w v s k
```

Interpret the effects of an `UI` component in a different monad.

#### `traversal`

``` purescript
traversal :: forall eff m v s t. (Monoid v, Functor m) => Traversal s s t t -> UI eff m v t t -> UI eff m v s s
```

Display a `UI` component for each focus of a `Traversal`.

#### `foreach`

``` purescript
foreach :: forall eff m v s t. (Monoid v, Traversable t, Functor m) => (Int -> UI eff m v s s) -> UI eff m v (t s) (t s)
```

Display a `UI` component for each element of a `Traversable` container,
with access to the index into the container.

#### `unsafeInline`

``` purescript
unsafeInline :: forall eff m v s t. Eff eff v -> UI eff m v s t
```

Create a `UI` component that executes an action while build.


