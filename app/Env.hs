{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Env
  ( Env (..),
    MonadReads,
  )
where

import Control.Monad.Reader
import Servant.Client (ClientEnv)
import Web.Telegram.API (Token (..))

data Env = Env {token :: Token, clientEnv :: ClientEnv}

type MonadReads env m = (MonadReader m, EnvType m ~ env)
