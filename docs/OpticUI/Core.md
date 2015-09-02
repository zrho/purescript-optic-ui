## Module OpticUI.Core

#### `UI`

``` purescript
newtype UI s eff a
```

##### Instances
``` purescript
instance uiFunctor :: Functor (UI s eff)
instance uiApplicative :: Applicative (UI s eff)
instance uiApply :: Apply (UI s eff)
instance uiBind :: Bind (UI s eff)
instance uiMonad :: Monad (UI s eff)
```

#### `runUI`

``` purescript
runUI :: forall s eff a. s -> (s -> Eff eff Unit) -> UI s eff a -> Eff eff (Either (Eff eff s) a)
```

#### `zoomOne`

``` purescript
zoomOne :: forall eff s t a. LensP s t -> UI t eff a -> UI s eff a
```

#### `zoomAll`

``` purescript
zoomAll :: forall eff s t a f. (Alternative f) => TraversalP s t -> UI t eff a -> UI s eff (f a)
```

Zoom in on a dynamic number of components using a traversal. The zoomed UI
component will be replicated for each target of the traversal in the
encompassing state and will have access to its respective image of the
traversal.

#### `zoomIxed`

``` purescript
zoomIxed :: forall eff s t a f. (Alternative f) => TraversalP s t -> (Int -> UI t eff a) -> UI s eff (f a)
```

Zoom in on a dynamic number of components using a traversal, like
[`zoomAll`](#zoomAll), keeping track of the index of the zoomed component.

Note: The time this function was published, the purescript lens library
did not yet support indexed traversals. Eventually, if that support is added,
this function will be altered to take an indexed traversal and pass the
respective index to the zoomed components.

#### `uiState`

``` purescript
uiState :: forall eff s. UI s eff s
```

Access the UI state as seen by this component.

#### `handler`

``` purescript
handler :: forall eff s e. (e -> s -> Eff eff s) -> UI s eff (e -> Eff eff Unit)
```

Create a continuation for an event handler function.

#### `execute`

``` purescript
execute :: forall eff s a. (s -> Eff eff s) -> UI s eff a
```

Execute some effectful computation that can change the state. The execution
of the current UI generation is aborted and the UI is rerendered from
scratch.

Be careful to change the state in a way such that this function is not
triggered again, which would result in an infinite loop.

#### `inline`

``` purescript
inline :: forall eff s a. (s -> Eff eff a) -> UI s eff Unit
```


