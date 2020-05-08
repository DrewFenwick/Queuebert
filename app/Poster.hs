{-# LANGUAGE TypeFamilies #-}

module Poster where

import           Control.Monad.Reader
import           Env

poster :: (MonadIO m, MonadReads Env m) => m ()
poster = undefined
