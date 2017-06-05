module Network.HTTP.Internal.Types
	( Request(Request)
	, Response(Response)
	, defRequest
	) where

import Prelude ((<>), class Show, show)

import Data.Foreign           (Foreign, toForeign)

foreign import undefined :: Foreign

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
	show (Request r) = "(Request {"
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
		<> " })"

newtype Response = Response
	{ statusCode :: Int
	, headers    :: Foreign -- ^ Rougly Map String String, with some (exceptions)[https://nodejs.org/api/http.html#http_message_headers]
	, body       :: String
	}

instance showResponse :: Show Response where
	show (Response r) = "(Response {"
		<> " statusCode: " <> show r.statusCode  <> ","
		<> " headers: "    <> "(Foreign *)"      <> ","
		<> " body: "       <> r.body             <> ","
		<> " })"

defRequest :: Request
defRequest = Request
	{ protocol: "http"
	, hostname: "localhost"
	, port    : 80
	, method  : "GET"
	, path    : "/"
	, headers : toForeign []
	, user    : undefined
	, password: undefined
	, body    : ""
	, timeout : undefined
	}
