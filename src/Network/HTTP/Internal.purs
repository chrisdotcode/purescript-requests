module Network.HTTP.Internal
	( toInternalRequest
	, fromInternalResponse
	, Platform(Node, Browser, Other)
	, determinePlatform
	) where

import Prelude (($), (<$>), (<>), (>>=), (<<<), map, show)

import Control.Alt                  ((<|>))
import Data.Array                   (catMaybes, head, last)
import Data.Foldable                (foldr)
import Data.Foreign                 (Foreign, Prop, toForeign, writeObject)
import Data.Foreign.Class           ((.=))
import Data.Foreign.Undefined       (writeUndefined)
import Data.List                    (intercalate, null)
import Data.List.NonEmpty           (fromFoldable, singleton, toList) as L
import Data.Map                     (fromFoldable, lookup, toList)
import Data.Maybe                   (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid                  (mempty)
import Data.String                  (Pattern(Pattern), split, joinWith)
import Data.Tuple                   (Tuple(Tuple))
import Data.URI                     (URI(URI))
import Data.URI.Authority           (Authority(Authority))
import Data.URI.HierarchicalPart    (HierarchicalPart(HierarchicalPart))
import Data.URI.Host                (printHost)
import Data.URI.Path                (printPath)
import Data.URI.Query               (printQuery)
import Data.URI.Scheme              (printScheme)
import Network.HTTP.Types.Cookie    (parse, stringify')
import Network.HTTP.Types.Exchange
	( Request(Request)
	, Response(Response)
	, Auth(BasicAuth)
	)
import Network.HTTP.Types.Header
	( Headers
	, HeaderName(Cookie, SetCookie)
	, HeaderValue(HVStr, HVList)
	, headerNameFromString
	) as H
import Network.HTTP.Types.StatusCode
	( StatusCode(StatusCode)
	, ReasonPhrase(Custom)
	, getRecognizedStatusCodeFromInt
	)

import Network.HTTP.Internal.Types
	( Request(Request)
	, Response(Response)
	, defRequest
	) as Internal

extractProtocol :: URI -> Maybe String
extractProtocol (URI protocol _ _ _) = printScheme <$> protocol

-- purescript-uri allows multiple hostnames, but we only grab the first one, as more often than not, there will only be one.
extractHostname :: URI -> Maybe String
extractHostname (URI s (HierarchicalPart (Just (Authority _ [(Tuple hostname _)])) _) _ _) = Just $ printHost hostname
extractHostname _ = Nothing

extractPort :: URI -> Maybe Int
extractPort (URI _ (HierarchicalPart (Just (Authority _ [(Tuple _ port)])) _) _ _) = port
extractPort _ = Nothing

extractPath :: URI -> String
extractPath (URI s (HierarchicalPart _ p) q f) =
	joinWith "" $ catMaybes
		[ printPath  <$> p
		, printQuery <$> q
		, ("#" <> _) <$> f
		]

toInternalHeaderValue :: H.HeaderValue -> String
toInternalHeaderValue (H.HVStr   s) = s
toInternalHeaderValue (H.HVList ss) = intercalate "," ss

toInternalHeaders :: H.Headers -> Array Prop
toInternalHeaders = foldr toInternalHeader [] <<< toList
	where
		toInternalHeader (Tuple name value) o = o <> [ show name .= toInternalHeaderValue value ]

extractUser :: String -> Maybe String
extractUser = head <<< split (Pattern ":")

-- XXX Assuming that there's only one ':' in the string.
extractPassword :: String -> Maybe String
extractPassword = last <<< split (Pattern ":")

-- XXX use purescript-newtype#unwrap?
extractRequestRecord (Internal.Request r) = r

extractAuth :: URI -> Maybe String
extractAuth (URI _ (HierarchicalPart (Just (Authority auth _)) _) _ _) = auth
extractAuth _                                                          = Nothing

toInternalRequest :: Request -> Internal.Request
toInternalRequest (Request r) = Internal.Request
	{ protocol: protocol'
	, hostname: fromMaybe defReq.hostname $
		extractHostname r.uri
	, port    : port'
	, method  : show r.method
	, path    : fromEmpty defReq.path $ extractPath r.uri
	, headers : writeObject $ toInternalHeaders r.headers <> cookies'
	-- If provided, the user's authentication takes precedence over authentication provided in the URI.
	, user    : maybeToForeign $
		(r.auth >>= getUser) <|> (auth' >>= extractUser) <|> Nothing
	, password: maybeToForeign $
		(r.auth >>= getPassword) <|> (auth' >>= extractPassword) <|> Nothing
	, body    : r.body
	, timeout : maybeToForeign r.timeout
	}
	where
		fromEmpty def "" = def
		fromEmpty _   s  = s

		maybeToForeign :: forall a. Maybe a -> Foreign
		maybeToForeign (Just x) = toForeign x
		maybeToForeign _        = writeUndefined

		-- XXX If the port is 80, and the protocol is 'https:' assume
		-- the the user left the default port unchanged, and change it
		-- for them. This will break things in the (rare?) occasion
		-- that the user is making an HTTPS request over port 80. I
		-- should fix this later (but probably won't until someone
		-- complains about it).
		messWithPort "https:" 80 = 443
		messWithPort _        p  = p

		defReq                      = extractRequestRecord Internal.defRequest
		protocol'                   = fromMaybe defReq.protocol $ extractProtocol r.uri
		port'                       = messWithPort protocol' $ fromMaybe defReq.port $ extractPort r.uri
		cookies'                    = if null r.cookies then [] else [ show H.Cookie .= stringify' r.cookies ]
		getUser     (BasicAuth u _) = u
		getPassword (BasicAuth _ p) = p
		auth'                       = extractAuth r.uri

foreign import foreignHeadersToHeadersImpl :: forall a b. (a -> b -> (Tuple a b)) -> -- Tuple constructor XXX Can/Should I make this type more specific here?
					      (String -> H.HeaderName)            -> -- HeaderName smart constructor
					      (String -> H.HeaderValue)           -> -- HVStr constructor
					      (Array String -> H.HeaderValue)     -> -- HVList from native Array "constructor"
					      Foreign                             ->
					      Array (Tuple H.HeaderName H.HeaderValue)

foreignHeadersToHeaders :: Foreign -> H.Headers
foreignHeadersToHeaders = fromFoldable <<< foreignHeadersToHeadersImpl
	Tuple
	H.headerNameFromString
	H.HVStr
	(H.HVList <<< fromMaybe (L.singleton "") <<< L.fromFoldable) -- XXX Each HTTP header should logically have a value, and we should never run into an empty case (even if it's the empty string, the value would be "". But if some miracle happens, return an empty value when building our HeaderValue)

fromInternalResponse :: Internal.Response -> Response
fromInternalResponse (Internal.Response r) = Response
	{ statusCode: fromMaybe customStatusCode $
		getRecognizedStatusCodeFromInt r.statusCode
	, headers   : headers'
	, cookies   : map parse $ fromHVList $ lookup H.SetCookie headers'
	, body      : r.body
	}
	where
		-- There may or may not be a way to extract the Reason Phrase from a response, but we'll forgo it for now.
		customStatusCode       = StatusCode { code: r.statusCode, reasonPhrase: Custom "" }
		headers'               = foreignHeadersToHeaders r.headers
		fromHVList (Just (H.HVList hs)) = L.toList hs
		fromHVList _                    = mempty

data Platform = Node | Browser | Other

foreign import determinePlatformImpl :: Platform -> -- `Node` constructor
					Platform -> -- `Browser` constructor
					Platform -> -- `Other` constructor
					Platform

determinePlatform :: Platform
determinePlatform = determinePlatformImpl Node Browser Other
