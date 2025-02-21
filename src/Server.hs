module Server (runServer) where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Control.Exception (bracket_)
import Data.ByteString.Lazy.Char8 (pack)

-- app :: Application
-- app _req respond = do
--     respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

app :: Application
app _req respond = bracket_
    (putStrLn "Allocating scarce resource")
    (putStrLn "Cleaning up")
    (respond $ responseLBS status200 [] (pack "Hello World"))

runServer :: IO ()
runServer = do
    putStrLn "Starting server on port 3000..."
    run 3000 app
