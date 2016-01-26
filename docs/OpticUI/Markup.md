## Module OpticUI.Markup

#### `TagName`

``` purescript
type TagName = String
```

#### `AttrName`

``` purescript
type AttrName = String
```

#### `PropName`

``` purescript
type PropName = String
```

#### `EventName`

``` purescript
type EventName = String
```

#### `NS`

``` purescript
type NS = String
```

#### `UniqueStr`

``` purescript
type UniqueStr = String
```

#### `Markup`

``` purescript
data Markup
  = Markup (Array Node)
```

##### Instances
``` purescript
instance markupSemigroup :: Semigroup Markup
instance markupMonoid :: Monoid Markup
```

#### `Node`

``` purescript
data Node
  = Text String
  | Element (Maybe NS) TagName (Array Prop) Markup
```

#### `element`

``` purescript
element :: Maybe NS -> TagName -> Array Prop -> Markup -> Markup
```

#### `text`

``` purescript
text :: String -> Markup
```

#### `Prop`

``` purescript
data Prop
  = AttrP AttrName String
  | PropP PropName (Exists PropE)
  | HandlerP EventName EventHandler
  | InitializerP UniqueStr Initializer
  | FinalizerP UniqueStr Finalizer
  | KeyP String
```

#### `PropE`

``` purescript
data PropE e
  = PropE e
```

#### `attr`

``` purescript
attr :: AttrName -> String -> Prop
```

#### `prop`

``` purescript
prop :: forall a. PropName -> a -> Prop
```

#### `handle`

``` purescript
handle :: forall e eff. EventName -> (e -> Eff eff Unit) -> Prop
```

#### `key`

``` purescript
key :: String -> Prop
```

#### `_KeyP`

``` purescript
_KeyP :: PrismP Prop String
```

#### `EventHandler`

``` purescript
data EventHandler
```

#### `mkEventHandler`

``` purescript
mkEventHandler :: forall e eff. (e -> Eff eff Unit) -> EventHandler
```

Build a handler from a handler function.

#### `runEventHandler`

``` purescript
runEventHandler :: forall r. (forall e eff. (e -> Eff eff Unit) -> r) -> EventHandler -> r
```

Use the handler function for a handler.

#### `Initializer`

``` purescript
data Initializer
```

#### `Finalizer`

``` purescript
data Finalizer
```

#### `mkInitializer`

``` purescript
mkInitializer :: forall eff. (HTMLElement -> Eff eff Unit) -> Initializer
```

#### `runInitializer`

``` purescript
runInitializer :: forall r. ((forall eff. HTMLElement -> Eff eff Unit) -> r) -> Initializer -> r
```

#### `initializer`

``` purescript
initializer :: forall eff. String -> (HTMLElement -> Eff eff Unit) -> Prop
```

#### `mkFinalizer`

``` purescript
mkFinalizer :: forall eff. (HTMLElement -> Eff eff Unit) -> Finalizer
```

#### `runFinalizer`

``` purescript
runFinalizer :: forall r. ((forall eff. HTMLElement -> Eff eff Unit) -> r) -> Finalizer -> r
```

#### `finalizer`

``` purescript
finalizer :: forall eff. String -> (HTMLElement -> Eff eff Unit) -> Prop
```

#### `Event`

``` purescript
type Event r = { bubbles :: Boolean, cancelable :: Boolean, currentTarget :: HTMLElement, target :: HTMLElement, timeStamp :: Number, type :: String | r }
```

#### `KeyboardEvent`

``` purescript
type KeyboardEvent r = Event (keyCode :: Int | r)
```


