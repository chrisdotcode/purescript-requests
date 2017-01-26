module Network.Responseable
	( module Network.Responseable
	, module Network.Responseable.Combinators
	, module Network.Responseable.Types
	) where

import Prelude ((<$>), (>>=), (>>>))

import Control.Monad.Aff           (Aff)
import Network.HTTP.Types.Methods  (Method(DELETE, GET, PATCH, POST ,PUT))

import Network.Responseable.Combinators (setBody, setMethod, uriToRequest)
import Network.Responseable.Types
	( Request(Request)
	, Response(Response)
	, defRequest
	)
import Network.Responseable.Internal
	( Platform(Node, Browser, Other)
	, determinePlatform
	, internalResponseToResponse
	, toInternalRequest
	)
import Network.Responseable.Internal.Node    (request) as Node
import Network.Responseable.Internal.Browser (request) as Browser

request :: forall e. Request -> Aff e Response
request req = internalResponseToResponse <$> case determinePlatform of
	Node    -> Node.request req'
	Browser -> Browser.request req'
	Other   -> Node.request req' -- XXX Other is not properly implemented yet.
	where
		req' = toInternalRequest req

get' :: forall e. Request -> Aff e Response
get' = setMethod GET >>> request

get :: forall e. String -> Aff e Response
get uri = uriToRequest uri >>= get'

post' :: forall e. Request -> Aff e Response
post' = setMethod POST >>> request

post :: forall e. String -> Aff e Response
post uri = uriToRequest uri >>= post'

postWithBody :: forall e. String -> String -> Aff e Response
postWithBody uri body = uriToRequest uri >>= setBody body >>> post'

put' :: forall e. Request -> Aff e Response
put' = setMethod PUT >>> request

put :: forall e. String -> Aff e Response
put uri = uriToRequest uri >>= put'

putWithBody :: forall e. String -> String -> Aff e Response
putWithBody uri body = uriToRequest uri >>= setBody body >>> put'

patch' :: forall e. Request -> Aff e Response
patch' = setMethod PATCH >>> request

patch :: forall e. String -> Aff e Response
patch uri = uriToRequest uri >>= patch'

patchWithBody :: forall e. String -> String -> Aff e Response
patchWithBody uri body = uriToRequest uri >>= setBody body >>> patch'

delete' :: forall e. Request -> Aff e Response
delete' = setMethod DELETE >>> request

delete :: forall e. String -> Aff e Response
delete uri = uriToRequest uri >>= delete'
