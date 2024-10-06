module HttpServer
  ( run
  ) where

import Config qualified
import Control.Monad (when)
import Data.Text qualified as Text
import Web.Scotty qualified as Scotty

run :: Config.EctExportConfig -> IO ()
run config =
  when config.enable do
    Scotty.scotty config.httpPort do
      Scotty.get "/" do
        Scotty.file $ Text.unpack config.outputPath
