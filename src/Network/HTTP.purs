module Network.HTTP
	( request
	, get'
	, get
	, post
	, post'
	, postWithBody
	, put
	, put'
	, putWithBody
	, patch
	, patch'
	, patchWithBody
	, delete
	, delete'
	) where

import Prelude ((<$>), (>>=), (>>>))

import Control.Monad.Aff          (Aff)
import Network.HTTP.Types.Exchange
	( Request
	, Response
	, delete
	, get
	, patch
	, patchWithBody
	, post
	, postWithBody
	, put
	, putWithBody
	, setMethod
	) as E
import Network.HTTP.Types.Method  (Method(DELETE, GET, PATCH, POST ,PUT))

import Network.HTTP.Internal
	( toInternalRequest
	, fromInternalResponse
	, Platform(Node, Browser, Other)
	, determinePlatform
	)
import Network.HTTP.Internal.Node    (request) as Node
import Network.HTTP.Internal.Browser (request) as Browser

request :: forall e. E.Request -> Aff e E.Response
request req = fromInternalResponse <$> case determinePlatform of
	Node    -> Node.request req'
	Browser -> Browser.request req'
	Other   -> Node.request req' -- XXX Other is not properly implemented yet.
	where
		req' = toInternalRequest req

get' :: forall e. E.Request -> Aff e E.Response
get' = E.setMethod GET >>> request

get :: forall e. String -> Aff e E.Response
get uri = E.get uri >>= request

post' :: forall e. E.Request -> Aff e E.Response
post' = E.setMethod POST >>> request

post :: forall e. String -> Aff e E.Response
post uri = E.post uri >>= request

postWithBody :: forall e. String -> String -> Aff e E.Response
postWithBody uri body = E.postWithBody uri body >>= request

put' :: forall e. E.Request -> Aff e E.Response
put' = E.setMethod PUT >>> request

put :: forall e. String -> Aff e E.Response
put uri = E.put uri >>= request

putWithBody :: forall e. String -> String -> Aff e E.Response
putWithBody uri body = E.putWithBody uri body >>= request

patch' :: forall e. E.Request -> Aff e E.Response
patch' = E.setMethod PATCH >>> request

patch :: forall e. String -> Aff e E.Response
patch uri = E.patch uri >>= request

patchWithBody :: forall e. String -> String -> Aff e E.Response
patchWithBody uri body = E.patchWithBody uri body >>= request

delete' :: forall e. E.Request -> Aff e E.Response
delete' = E.setMethod DELETE >>> request

delete :: forall e. String -> Aff e E.Response
delete uri = E.delete uri >>= request
