## Module OpticUI.Internal.VirtualDOM

#### `VTree`

``` purescript
data VTree
```

#### `Patch`

``` purescript
data Patch
```

#### `Props`

``` purescript
data Props
```

##### Instances
``` purescript
instance semigroupProps :: Semigroup Props
instance monoidProps :: Monoid Props
```

#### `createElement`

``` purescript
createElement :: VTree -> HTMLElement
```

#### `diff`

``` purescript
diff :: VTree -> VTree -> Patch
```

#### `patch`

``` purescript
patch :: forall eff. Patch -> HTMLElement -> Eff (dom :: DOM | eff) HTMLElement
```

#### `vtext`

``` purescript
vtext :: String -> VTree
```

#### `vnode`

``` purescript
vnode :: Nullable String -> String -> Nullable String -> Props -> Array VTree -> VTree
```

#### `prop`

``` purescript
prop :: forall a. Fn2 String a Props
```

#### `attrProp`

``` purescript
attrProp :: Fn2 String String Props
```

#### `handlerProp`

``` purescript
handlerProp :: forall eff e. Fn2 String (e -> Eff eff Unit) Props
```

#### `concatProps`

``` purescript
concatProps :: Fn2 Props Props Props
```

#### `emptyProps`

``` purescript
emptyProps :: Props
```

#### `initializer`

``` purescript
initializer :: forall eff. Fn2 String (HTMLElement -> Eff eff Unit) Props
```

#### `finalizer`

``` purescript
finalizer :: forall eff. Fn2 String (HTMLElement -> Eff eff Unit) Props
```


