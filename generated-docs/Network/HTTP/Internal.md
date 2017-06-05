## Module Network.HTTP.Internal

#### `toInternalRequest`

``` purescript
toInternalRequest :: Request -> Request
```

#### `fromInternalResponse`

``` purescript
fromInternalResponse :: Response -> Response
```

#### `Platform`

``` purescript
data Platform
  = Node
  | Browser
  | Other
```

#### `determinePlatform`

``` purescript
determinePlatform :: Platform
```


