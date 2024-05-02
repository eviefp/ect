module HttpServer
    ( run
    ) where

import Config qualified
import Control.Monad (when)
import Control.Monad.IO.Class qualified as IO
import Data.ByteString.Lazy qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Types qualified as Status
import Network.Wai.Handler.Warp qualified as WAI
import Web.Twain qualified as T

run :: Config.EctExportConfig -> IO ()
run config =
    when config.enable
        . WAI.run config.httpPort
        . routes
        $ [ T.get "/" (index config.outputPath)
          ]

index :: Text -> T.ResponderM a
index path = do
    content <- IO.liftIO $ BS.readFile $ Text.unpack path
    T.send . T.raw Status.ok200 [] $ content

routes :: [T.Middleware] -> T.Application
routes = foldr ($) empty
  where
    empty :: T.Application
    empty = T.notFound . T.send . T.raw Status.notFound404 [] $ ""
