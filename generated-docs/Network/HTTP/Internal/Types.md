## Module Network.HTTP.Internal.Types

#### `Request`

``` purescript
newtype Request
  = Request { protocol :: String, hostname :: String, port :: Int, method :: String, path :: String, headers :: Foreign, user :: Foreign, password :: Foreign, body :: String, timeout :: Foreign }
```

##### Instances
``` purescript
Show Request
```

#### `Response`

``` purescript
newtype Response
  = Response { statusCode :: Int, headers :: Foreign, body :: String }
```

##### Instances
``` purescript
Show Response
```

#### `defRequest`

``` purescript
defRequest :: Request
```


