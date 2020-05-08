{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API
  ( getUpdates
  , runClientM
  , sendPhoto
  )
where

import           Control.Monad.Reader
import           Data.Proxy
import           Data.Text
import           Env                            ( Env
                                                , MonadReads
                                                )
import qualified Env
import           Servant.API
import           Servant.Client                 ( ClientError
                                                , ClientM
                                                )
import qualified Servant.Client                as Servant
import           Web.Telegram.API
import           Web.Telegram.Types
import           Web.Telegram.Types.Update

type API = GetUpdates :<|> SendPhoto

api :: Proxy API
api = Proxy

sendPhoto :: Token -> PhotoMessage Text -> ClientM (ReqResult Message)

getUpdates :: Token -> Polling -> ClientM (ReqResult [Update])
getUpdates :<|> sendPhoto = Servant.client api

runClientM
  :: (MonadReads Env m, MonadIO m) => ClientM a -> m (Either ClientError a)
runClientM clientM = do
  cliEnv <- asks Env.clientEnv
  liftIO $ Servant.runClientM clientM cliEnv
