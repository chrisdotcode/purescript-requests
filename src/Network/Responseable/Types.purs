module Network.Responseable.Types
	( Auth(Auth)
	, Cookie(Cookie)
	, Request(Request)
	, Response(Response)
	, defRequest
	, defRequest'
	, defURI
	) where

import Prelude ((<>), class Show, show)

import Data.DateTime                  (DateTime)
import Data.DateTime.Instant          (Instant)
import Data.Either                    (Either(Left))
import Data.List                      (List)
import Data.Maybe                     (Maybe(Just, Nothing))
import Data.Monoid                    (mempty)
import Data.Path.Pathy                (Abs, Unsandboxed, rootDir)
import Data.Tuple                     (Tuple(Tuple))
import Data.URI                       (URI(URI))
import Data.URI.Authority             (Authority(Authority))
import Data.URI.HierarchicalPart      (HierarchicalPart(HierarchicalPart))
import Data.URI.Host                  (Host(NameAddress))
import Data.URI.Path                  (URIPath)
import Data.URI.Scheme                (URIScheme(URIScheme))
import Network.HTTP.Types.Headers     (Headers)
import Network.HTTP.Types.StatusCodes (StatusCode)
import Network.HTTP.Types.Methods     (Method(GET))

newtype Cookie = Cookie
	{ name     :: String
	, value    :: String
	, domain   :: Maybe Host
	, expires  :: Maybe DateTime
	, httpOnly :: Maybe Boolean
	, maxAge   :: Maybe Instant
	, path     :: URIPath Abs Unsandboxed
	, secure   :: Maybe Boolean
	}

instance showCookie :: Show Cookie where
	show (Cookie c) = "Cookie {"
		<> " name:"       <> show c.name     <> ","
		<> " value:"      <> show c.value    <> ","
		<> " domain: ("   <> show c.domain   <> "),"
		<> " expires: ("  <> show c.expires  <> "),"
		<> " httpOnly: (" <> show c.httpOnly <> "),"
		<> " maxAge: ("   <> show c.maxAge   <> "),"
		<> " path: ("     <> show c.path     <> "),"
		<> " secure: ("   <> show c.secure   <> "),"
		<> " }"

newtype Auth = Auth
	{ user     :: Maybe String
	, password :: Maybe String
	}

instance showAuth :: Show Auth where
	show (Auth a) = "Auth {"
		<> " user: ("     <> show a.user     <> "),"
		<> " password: (" <> show a.password <> "),"
		<> " }"

newtype Request = Request
	{ uri     :: URI
	, method  :: Method
	, headers :: Headers
	, cookies :: List Cookie
	, auth    :: Maybe Auth -- ^ If Auth information is provided in both the request.uri and request.auth, the auth in request.auth is preferred.
	, body    :: String
	, timeout :: Maybe Int -- Really a UInt
	}

instance showRequest :: Show Request where
	show (Request r) = "Request {"
		<> " uri: ("     <> show r.uri     <> "),"
		<> " method: ("  <> show r.method  <> "),"
		<> " headers: (" <> show r.headers <> "),"
		<> " cookies: (" <> show r.cookies <> "),"
		<> " auth: ("    <> show r.auth    <> "),"
		<> " body:"      <> show r.body    <> ","
		<> " timeout: (" <> show r.timeout <> "),"
		<> " }"

newtype Response = Response
	{ statusCode :: StatusCode
	, headers    :: Headers
	, body       :: String
	, cookies    :: List Cookie
	}

instance showResponse :: Show Response where
	show (Response r) = "Response {"
		<> " statusCode: (" <> show r.statusCode <> "),"
		<> " headers: ("    <> show r.headers    <> "),"
		<> " body:"         <> show r.body       <> ","
		<> " cookies: ("    <> show r.cookies    <> "),"
		<> " }"

defURI :: URI
defURI = URI
	(Just (URIScheme "http"))
	(HierarchicalPart (Just (Authority Nothing
	[(Tuple (NameAddress "localhost")
	(Just 80))]))
	(Just (Left rootDir)))
	Nothing
	Nothing

defRequest ::
	{ uri :: URI                          
	, method :: Method                    
	, headers :: Headers
	, cookies :: List Cookie
	, auth :: Maybe Auth
	, body :: String                      
	, timeout :: Maybe Int
	}
defRequest =
	{ uri    : defURI
	, method : GET
	, headers: mempty
	, cookies: mempty
	, auth   : Nothing
	, body   : ""
	, timeout: Nothing
	}

defRequest' :: Request
defRequest' = Request defRequest
