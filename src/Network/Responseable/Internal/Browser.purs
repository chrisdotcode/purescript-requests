module Network.Responseable.Internal.Browser (request) where

import Prelude           (($), Unit)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Data.Either       (Either(Left, Right))

import Network.Responseable.Internal.Types (InternalRequest, InternalResponse)

foreign import requestImpl :: forall e. (String -> Either String InternalResponse) -> -- ^ Left function
			      (InternalResponse -> Either String InternalResponse) -> -- ^ Right function
			      (Either String InternalResponse -> Eff e Unit)       ->
			      InternalRequest                                      ->
			      Eff e Unit

requestImpl' :: forall e. (Either String InternalResponse -> Eff e Unit) ->
		InternalRequest                                ->
		Eff e Unit
requestImpl' = requestImpl Left Right

request :: forall e. InternalRequest -> Aff e (Either String InternalResponse)
request req = makeAff $ \_ callback -> requestImpl' callback req

