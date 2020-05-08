{-# LANGUAGE TypeFamilies #-}

module Listener where

import API
import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Data.Functor (($>))
import Env (Env, MonadReads)
import qualified Env
import Web.Telegram.API (Token)
import Web.Telegram.API.Update (Polling (..))
import Web.Telegram.Types.Update (ReqResult (..), Update)

listener :: (MonadIO m, MonadReads Env m) => m ()
listener = do
  go <- asks (listenerLoop . Env.token)
  uid <- getLastUpdate
  go uid

listenerLoop :: (MonadIO m, MonadReads Env m) => Token -> Int -> m ()
listenerLoop token uid = do
  result <- runClientM $ getUpdates token (polling uid)
  newUid <- case result of
    Right (Ok updates) -> store updates $> undefined updates
    Left err -> liftIO (print err) $> uid
  liftIO (threadDelay 10000)
  listenerLoop token newUid

store :: (MonadIO m, MonadReads Env m) => [Update] -> m ()
store = undefined

getLastUpdate :: (MonadIO m, MonadReads Env m) => m Int
getLastUpdate = undefined

polling :: Int -> Polling
polling n =
  Polling
    { offset = Just n,
      limit = Nothing,
      timeout = Nothing,
      allowedUpdates = Nothing
    }
