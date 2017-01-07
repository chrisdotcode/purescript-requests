module Network.Responseable.Internal.Browser (request) where

import Prelude (($), Unit)

import Control.Monad.Aff           (Aff, makeAff)
import Control.Monad.Eff           (Eff)
import Control.Monad.Eff.Exception (Error)

import Network.Responseable.Internal.Types (Request, Response)

foreign import requestImpl :: forall e. (Error    -> Eff e Unit) ->
					(Response -> Eff e Unit) ->
					Request                  ->
					Eff e Unit

request :: forall e. Request -> Aff e Response
request req = makeAff $ \errback callback -> requestImpl errback callback req
