module Network.Responseable.Combinators where

import Prelude
	( ($)
	, (<>)
	, (<$>)
	, (>>>)
	, (<<<)
	, class Show
	, const
	, flip
	, pure
	, show
	)

import Control.Monad.Eff.Exception    (Error, error)
import Control.Monad.Error.Class      (class MonadError, throwError)
import Control.Monad.Except           (Except, runExcept)
import Data.DateTime                  (DateTime)
import Data.Time.Duration             (Milliseconds)
import Data.DateTime.Instant          (Instant)
import Data.Either                    (either)
import Data.Foreign                   (Foreign, parseJSON)
import Data.Foreign.Class             (class IsForeign, readJSON)
import Data.List                      (List, singleton)
import Data.Maybe                     (Maybe(Just, Nothing))
import Data.Path.Pathy                (Abs, Unsandboxed)
import Data.Tuple                     (Tuple(Tuple))
import Data.URI                       (URI(URI), runParseURI)
import Data.URI.Authority             (Authority(Authority))
import Data.URI.Fragment              (Fragment)
import Data.URI.HierarchicalPart      (HierarchicalPart(HierarchicalPart))
import Data.URI.Host                  (Host)
import Data.URI.Path                  (URIPath, URIPathAbs)
import Data.URI.Scheme                (URIScheme(URIScheme))
import Data.URI.Types                 (Port)
import Data.URI.Query                 (Query(Query))
import Network.HTTP.Types.Headers     (Headers)
import Network.HTTP.Types.Methods     (Method(DELETE, GET, PATCH, POST ,PUT))
import Network.HTTP.Types.StatusCodes (StatusCode)
import Text.Parsing.StringParser      (ParseError(ParseError))

import Network.Responseable.Types
	( Auth(BasicAuth)
	, Cookie(Cookie)
	, Request(Request)
	, Response(Response)
	, defRequest
	)

basicAuth :: String -> String -> Auth
basicAuth user password = BasicAuth (Just user) (Just password)

http :: URIScheme
http = URIScheme "http"

https :: URIScheme
https = URIScheme "https"

makeQuery :: String -> String -> Query
makeQuery key = Query <<< singleton <<< Tuple key <<< Just

makeQuery' :: String -> Query
makeQuery' = Query <<< singleton <<< flip Tuple Nothing

-- Kinda-sorta a Monoid over Query.
joinQuery :: Query -> Query -> Query
joinQuery (Query l1) (Query l2) = Query (l1 <> l2)

-- Lenses, eventually?
-- XXX I hate having different names like this. Maybe create Combinators.`Type`
-- modules, and just import them in this one.
setCookieName :: String -> Cookie -> Cookie
setCookieName name (Cookie c) = Cookie c { name = name }

setCookieValue :: String -> Cookie -> Cookie
setCookieValue value (Cookie c) = Cookie c { value = value }

setCookieDomain :: Maybe Host -> Cookie -> Cookie
setCookieDomain domain (Cookie c) = Cookie c { domain = domain }

setCookieExpires :: Maybe DateTime -> Cookie -> Cookie
setCookieExpires expires (Cookie c) = Cookie c { expires = expires }

setCookieHttpOnly :: Maybe Boolean -> Cookie -> Cookie
setCookieHttpOnly httpOnly (Cookie c) = Cookie c { httpOnly = httpOnly }

setCookieMaxAge :: Maybe Instant -> Cookie -> Cookie
setCookieMaxAge maxAge (Cookie c) = Cookie c { maxAge = maxAge }

setCookiePath :: URIPath Abs Unsandboxed -> Cookie -> Cookie
setCookiePath path (Cookie c) = Cookie c { path = path }

setCookieSecure :: Maybe Boolean -> Cookie -> Cookie
setCookieSecure secure (Cookie c) = Cookie c { secure = secure }

setUser :: String -> Auth -> Auth
setUser user (BasicAuth _ p) = BasicAuth (Just user) p

setPassword :: String -> Auth -> Auth
setPassword password (BasicAuth u _) = BasicAuth u (Just password)

setProtocol :: URIScheme -> Request -> Request
setProtocol protocol (Request r@{ uri: (URI uriScheme h q f) }) =
	Request r { uri = URI (Just protocol) h q f }

setCookieAuth :: Auth -> Request -> Request
setCookieAuth auth (Request r) = Request r { auth = Just auth }

setHostname :: Host -> Request -> Request
setHostname hostname (Request r@{ uri: (URI s (HierarchicalPart (Just (Authority u [(Tuple _ p)])) u') q f) }) =
	Request r { uri = URI s (HierarchicalPart (Just (Authority u [(Tuple hostname p)])) u') q f }
setHostname _ r = r

setPort :: Port -> Request -> Request
setPort port (Request r@{ uri: (URI s (HierarchicalPart (Just (Authority u [(Tuple h _)])) u') q f) }) =
	Request r { uri = URI s (HierarchicalPart (Just (Authority u [(Tuple h (Just port))])) u') q f }
setPort _ r = r

modifyPath :: (URIPathAbs -> URIPathAbs) -> Request -> Request
modifyPath fn (Request r@{ uri: (URI s (HierarchicalPart a (Just u)) q f) }) =
	Request r { uri = URI s (HierarchicalPart a (Just $ fn u)) q f }
modifyPath _ r                                                               = r

setPath :: URIPathAbs -> Request -> Request
setPath = modifyPath <<< const

modifyQuery :: (Query -> Query) -> Request -> Request
modifyQuery fn (Request r@{ uri: (URI s h (Just q) f) }) =
	Request r { uri = URI s h (Just $ fn q) f }
modifyQuery _ r = r

setQuery :: Query -> Request -> Request
setQuery = modifyQuery <<< const

modifyFragment :: (Fragment -> Fragment) -> Request -> Request
modifyFragment fn (Request r@{uri: (URI s h q (Just f)) }) =
	Request r { uri = URI s h q (Just $ fn f) }
modifyFragment _ r                                         = r

setFragment :: Fragment -> Request -> Request
setFragment = modifyFragment <<< const

setURI :: URI -> Request -> Request
setURI uri (Request r) = Request r { uri = uri }

setMethod :: Method -> Request -> Request
setMethod method (Request r) = Request r { method = method }

setHeaders :: Headers -> Request -> Request
setHeaders headers (Request r) = Request r { headers = headers }

setCookies :: List Cookie -> Request -> Request
setCookies cookies (Request r) = Request r { cookies = cookies }

setAuth :: Maybe Auth -> Request -> Request
setAuth auth (Request r) = Request r { auth = auth }

setBody :: String -> Request -> Request
setBody body (Request r) = Request r { body = body }

setTimeout :: Maybe Milliseconds -> Request -> Request
setTimeout timeout (Request r) = Request r { timeout = timeout }

setResponseStatusCode :: StatusCode -> Response -> Response
setResponseStatusCode statusCode (Response r) = Response r { statusCode = statusCode }

setResponseHeaders :: Headers -> Response -> Response
setResponseHeaders headers (Response r) = Response r { headers = headers }

setResponseBody :: String -> Response -> Response
setResponseBody body (Response r) = Response r { body = body }

setResponseCookies :: List Cookie -> Response -> Response
setResponseCookies cookies (Response r) = Response r { cookies = cookies }

uriToRequest' :: URI -> Request
uriToRequest' uri = Request defRequest { uri = uri }

uriToRequest :: forall m. MonadError Error m => String -> m Request
uriToRequest = runParseURI >>>
	either (extractErrorMessage >>> error >>> throwError)
	       (uriToRequest' >>> pure)
	where
		extractErrorMessage (ParseError msg) = msg

uriToMaybeRequest :: String -> Maybe Request
uriToMaybeRequest = runParseURI >>>
	either (const Nothing) (uriToRequest' >>> pure)

get :: forall m. MonadError Error m => String -> m Request
get uri = setMethod GET <$> uriToRequest uri

post :: forall m. MonadError Error m => String -> m Request
post uri = setMethod POST <$> uriToRequest uri

postWithBody :: forall m. MonadError Error m => String -> String -> m Request
postWithBody uri body = setBody body <$> post uri

put :: forall m. MonadError Error m => String -> m Request
put uri = setMethod PUT <$> uriToRequest uri

putWithBody :: forall m. MonadError Error m => String -> String -> m Request
putWithBody uri body = setBody body <$> put uri

patch :: forall m. MonadError Error m => String -> m Request
patch uri = setMethod PATCH <$> uriToRequest uri

patchWithBody :: forall m. MonadError Error m => String -> String -> m Request
patchWithBody uri body = setBody body <$> patch uri

delete :: forall m. MonadError Error m => String -> m Request
delete uri = setMethod DELETE <$> uriToRequest uri

-- XXX For now, we just take the array of errors, `show` them together, and
-- then throw them as an error message. I might want to revisit how I display
-- these error messages in the future.
fromJSONWith :: forall m a e. (MonadError Error m, Show e) =>
		(String -> Except e a)                     ->
		Response                                   ->
		m a
fromJSONWith fn (Response r) =
	either (throwError <<< error <<< show) pure $ runExcept $ fn r.body

fromJSON :: forall m a. (MonadError Error m, IsForeign a) => Response -> m a
fromJSON = fromJSONWith readJSON

asJSON :: forall m. MonadError Error m => Response -> m Foreign
asJSON = fromJSONWith parseJSON
