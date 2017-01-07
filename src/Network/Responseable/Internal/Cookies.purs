module Network.Responseable.Internal.Cookies (parse) where

import Network.Responseable.Internal.Types (Cookie) as Internal

foreign import parseImpl :: String -> Internal.Cookie

parse :: String -> Internal.Cookie
parse = parseImpl
