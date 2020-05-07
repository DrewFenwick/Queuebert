{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Env
  ( Env (..),
    MonadReads,
    App,
    runApp,
  )
where

import Control.Monad.Reader
import Data.Text (pack)
import Servant.Client (ClientEnv)
import System.Environment (getEnv)
import Web.Telegram.API (Token (..))

data Env = Env {token :: Token, clientEnv :: ClientEnv}

type MonadReads env m = (MonadReader m, EnvType m ~ env)

type App a = ReaderT Env IO a

runApp :: App a -> IO a
runApp app = do
  env <- mkEnv
  runReaderT app env

mkEnv :: IO Env
mkEnv = do
  tok <- Token . pack <$> getEnv "QueuebertToken"
  pure $ Env tok undefined
