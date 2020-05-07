module Env
  ( Env (..),
  )
where

import Servant.Client (ClientEnv)
import Web.Telegram.API (Token (..))

data Env = Env {token :: Token, clientEnv :: ClientEnv}
