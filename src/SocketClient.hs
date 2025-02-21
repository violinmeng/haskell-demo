module SocketClient (runSocketClient) where

import Network.Socket
import Network.Socket.ByteString (recv, send)
import qualified Data.ByteString.Char8 as C
import Control.Exception (bracket)

-- 客户端配置
host :: String
host = "127.0.0.1"

port :: PortNumber
port = 3001

-- 运行客户端
runSocketClient :: String -> IO String
runSocketClient message = withSocketsDo $ do
    putStrLn $ "Connecting to " ++ host ++ ":" ++ show port
    -- 创建socket并连接到服务器
    bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
        connect sock (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))
        putStrLn "Connected to server"

        -- 发送消息
        _ <-send sock (C.pack message)
        putStrLn $ "Sent: " ++ message

        -- 接收响应
        response <- recv sock 1024
        let responseStr = C.unpack response
        putStrLn $ "Received: " ++ responseStr
        return responseStr
