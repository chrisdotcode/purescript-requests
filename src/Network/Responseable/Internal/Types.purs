module Network.Responseable.Internal.Types
	( InternalRequest
	, InternalResponse
	) where

import Data.Foreign           (Foreign)
import Data.Foreign.Undefined (Undefined)

newtype InternalRequest = InternalRequest
	{ protocol :: String
	, hostname :: String
	, port     :: Int
	, method   :: String
	, path     :: String
	, headers  :: Foreign -- ^ Map String String
	, user     :: Undefined String
	, password :: Undefined String
	, timeout  :: Undefined Int
	, body     :: String
	}

newtype InternalResponse = InternalResponse
	{ statusCode :: Int
	, headers    :: Foreign -- ^ Rougly Map String String, with some (exceptions)[https://nodejs.org/api/http.html#http_message_headers]
	, body       :: String
	}
