{-# LANGUAGE TypeFamilies #-}

module Listener where

import API
import Control.Monad.Reader
import Data.Foldable
import Env (Env, MonadReads)
import qualified Env
import Web.Telegram.API.Update (Polling (..))
import Web.Telegram.Types.Update (ReqResult (..))

listener :: (MonadIO m, MonadReads Env m) => m ()
listener = do
  token <- asks Env.token
  result <- runClientM $ getUpdates token polling
  case result of
    Right (Ok updates) -> liftIO $ traverse_ (print) updates
    Left err -> liftIO $ print err

polling :: Polling
polling =
  Polling
    { offset = Nothing,
      limit = Nothing,
      timeout = Nothing,
      allowedUpdates = Nothing
    }
