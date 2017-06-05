## Module Network.HTTP

#### `request`

``` purescript
request :: forall e. Request -> Aff e Response
```

#### `get'`

``` purescript
get' :: forall e. Request -> Aff e Response
```

#### `get`

``` purescript
get :: forall e. String -> Aff e Response
```

#### `post'`

``` purescript
post' :: forall e. Request -> Aff e Response
```

#### `post`

``` purescript
post :: forall e. String -> Aff e Response
```

#### `postWithBody`

``` purescript
postWithBody :: forall e. String -> String -> Aff e Response
```

#### `put'`

``` purescript
put' :: forall e. Request -> Aff e Response
```

#### `put`

``` purescript
put :: forall e. String -> Aff e Response
```

#### `putWithBody`

``` purescript
putWithBody :: forall e. String -> String -> Aff e Response
```

#### `patch'`

``` purescript
patch' :: forall e. Request -> Aff e Response
```

#### `patch`

``` purescript
patch :: forall e. String -> Aff e Response
```

#### `patchWithBody`

``` purescript
patchWithBody :: forall e. String -> String -> Aff e Response
```

#### `delete'`

``` purescript
delete' :: forall e. Request -> Aff e Response
```

#### `delete`

``` purescript
delete :: forall e. String -> Aff e Response
```


