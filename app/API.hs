{-# LANGUAGE TypeFamilies #-}

module API
  ( getUpdates,
    runClientM,
  )
where

import Control.Monad.Reader
import Data.Proxy
import Env (Env, MonadReads)
import qualified Env
import Servant.Client (ClientError, ClientM)
import qualified Servant.Client as Servant
import Web.Telegram.API
import Web.Telegram.Types.Update

type API = GetUpdates

api :: Proxy API
api = Proxy

getUpdates :: Token -> Polling -> ClientM (ReqResult [Update])
getUpdates = Servant.client api

runClientM ::
  (MonadReads Env m, MonadIO m) =>
  ClientM a ->
  m (Either ClientError a)
runClientM clientM = do
  cliEnv <- asks Env.clientEnv
  liftIO $ Servant.runClientM clientM cliEnv
