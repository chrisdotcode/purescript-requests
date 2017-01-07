module Network.Responseable.Internal.Types
	( Cookie(Cookie)
	, Request(Request)
	, Response(Response)
	, defRequest
	) where

import Prelude ((<>), class Show, show)

import Data.Foreign           (Foreign, writeObject)
import Data.Foreign.Undefined (writeUndefined)

newtype Request = Request
	{ protocol :: String
	, hostname :: String
	, port     :: Int
	, method   :: String
	, path     :: String
	, headers  :: Foreign -- ^ Map String String
	, user     :: Foreign -- ^ Undefined String
	, password :: Foreign -- ^ Undefined String
	, body     :: String
	, timeout  :: Foreign -- ^ Undefined Int
	}

instance showRequest :: Show Request where
	show (Request r) = "Request {"
		<> " protocol: " <> r.protocol    <> ","
		<> " hostname: " <> r.hostname    <> ","
		<> " port: "     <> show r.port   <> ","
		<> " method: "   <> r.method      <> ","
		<> " path: "     <> r.path        <> ","
		<> " headers: "  <> "(Foreign *)" <> ","
		<> " user: "     <> "(Foreign *)" <> ","
		<> " password: " <> "(Foreign *)" <> ","
		<> " body: "     <> r.body        <> ","
		<> " timeout: "  <> "(Foreign *)" <> ","
		<> " }"

newtype Response = Response
	{ statusCode :: Int
	, headers    :: Foreign -- ^ Rougly Map String String, with some (exceptions)[https://nodejs.org/api/http.html#http_message_headers]
	, body       :: String
	}

instance showResponse :: Show Response where
	show (Response r) = "Response {"
		<> " statusCode: " <> show r.statusCode  <> ","
		<> " headers: "    <> "(Foreign *)"      <> ","
		<> " body: "       <> r.body             <> ","
		<> " }"

newtype Cookie = Cookie
	{ key      :: String
	, value    :: String
	-- You would expect the following fields to have the types that are
	-- commented on them, however those are what the PureScript
	-- representation of the type would be. Since JS has no types, we
	-- represent them as whatever JS gives back, which is `Undefined|a`,
	-- a.k.a., `Foreign`.
	, domain   :: Foreign -- ^ Undefined String
	, expires  :: Foreign -- ^ Undefined JSDate
	, httpOnly :: Foreign -- ^ Undefined Boolean
	, maxAge   :: Foreign -- ^ Undefined Number
	, path     :: Foreign -- ^ Undefined String
	, secure   :: Foreign -- ^ Undefined Boolean
	}

instance showCookie :: Show Cookie where
	show (Cookie c) = "Cookie {"
		<> " key: "      <> c.key         <> ","
		<> " value: "    <> c.value       <> ","
		<> " domain: "   <> "(Foreign *)" <> ","
		<> " expires: "  <> "(Foreign *)" <> ","
		<> " httpOnly: " <> "(Foreign *)" <> ","
		<> " maxAge: "   <> "(Foreign *)" <> ","
		<> " path: "     <> "(Foreign *)" <> ","
		<> " secure: "   <> "(Foreign *)" <> ","
		<> " }"

defRequest :: Request
defRequest = Request
	{ protocol: "http"
	, hostname: "localhost"
	, port    : 80
	, method  : "GET"
	, path    : "/"
	, headers : writeObject []
	, user    : writeUndefined
	, password: writeUndefined
	, body    : ""
	, timeout : writeUndefined
	}
