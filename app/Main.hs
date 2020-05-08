module Main where

import           Control.Concurrent
import           Control.Monad.Reader
import           Env
import           Listener
import           Poster

main :: IO ()
main = runApp app

app :: App ()
app = do
  _ <- mapReaderT forkIO poster
  listener
