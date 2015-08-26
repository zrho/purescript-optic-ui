## Module OpticUI.HTML

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

#### `HTML`

``` purescript
data HTML eff
  = Text String
  | Element (Maybe NS) TagName (Array (Prop eff)) (Array (HTML eff))
```

#### `HandlerE`

``` purescript
data HandlerE eff e
  = HandlerE EventName (e -> Eff eff Unit)
```

#### `PropE`

``` purescript
data PropE e
  = PropE String e
```

#### `Prop`

``` purescript
data Prop eff
  = Attr AttrName String
  | Prop (Exists PropE)
  | Handler (Exists (HandlerE eff))
```

#### `attr`

``` purescript
attr :: forall eff. AttrName -> String -> Prop eff
```

#### `prop`

``` purescript
prop :: forall eff a. PropName -> a -> Prop eff
```

#### `handle`

``` purescript
handle :: forall e eff. EventName -> (e -> Eff eff Unit) -> Prop eff
```

#### `Event`

``` purescript
type Event r = { bubbles :: Boolean, cancelable :: Boolean, currentTarget :: HTMLElement, target :: HTMLElement, timeStamp :: Number, type :: String | r }
```


