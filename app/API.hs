module API
  ( getUpdates,
  )
where

import Data.Proxy
import Servant.Client (ClientError, ClientM)
import qualified Servant.Client as Servant
import Web.Telegram.API
import Web.Telegram.Types.Update

type API = GetUpdates

api :: Proxy API
api = Proxy

getUpdates :: Token -> Polling -> ClientM (ReqResult [Update])
getUpdates = Servant.client api
