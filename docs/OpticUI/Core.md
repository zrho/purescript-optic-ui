## Module OpticUI.Core

#### `UI`

``` purescript
newtype UI s eff a
  = UI (Tuple s (s -> Eff eff Unit) -> Eff eff a)
```

##### Instances
``` purescript
instance uiFunctor :: Functor (UI s eff)
instance uiApplicative :: Applicative (UI s eff)
instance uiApply :: Apply (UI s eff)
instance uiBind :: Bind (UI s eff)
instance uiMonad :: Monad (UI s eff)
```

#### `zoomOne`

``` purescript
zoomOne :: forall eff s t a. LensP s t -> UI t eff a -> UI s eff a
```

#### `zoomAll`

``` purescript
zoomAll :: forall eff s t a. TraversalP s t -> UI t eff a -> UI s eff (List a)
```

Zoom in on a dynamic number of components using a traversal. The zoomed UI
component will be replicated for each target of the traversal in the
encompassing state and will have access to its respective image of the
traversal.

#### `zoomIxed`

``` purescript
zoomIxed :: forall eff s t a. TraversalP s t -> (Int -> UI t eff a) -> UI s eff (List a)
```

Zoom in on a dynamic number of components using a traversal, like
[`zoomAll`](#zoomAll), keeping track of the index of the zoomed component.

Note: The time this function was published, the purescript lens library
did not yet support indexed traversals. Eventually, if that support is added,
this function will be altered to take an indexed traversal and pass the
respective index to the zoomed components.

#### `handler`

``` purescript
handler :: forall eff s e. (e -> s -> Eff eff s) -> UI s eff (e -> Eff eff Unit)
```

Create a continuation for an event handler function.

#### `uiState`

``` purescript
uiState :: forall eff s. UI s eff s
```

Access the UI state as seen by this component.


