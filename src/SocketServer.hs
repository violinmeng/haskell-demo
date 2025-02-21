module SocketServer (runSocketServer) where

import Network.Socket
import Network.Socket.ByteString (recv, send)
import qualified Data.ByteString.Char8 as C
import Control.Concurrent (forkFinally)
import Control.Exception (bracket)
import Data.Char (toUpper)

-- 服务器配置
port :: PortNumber
port = 3001

-- 处理单个客户端连接
handleClient :: Socket -> IO ()
handleClient conn = do
    msg <- recv conn 1024  -- 接收最多1024字节
    if C.null msg
        then return ()
        else do
            -- 将接收到的消息转换为大写并发送回客户端
            let response = C.map toUpper msg
            _ <- send conn response
            handleClient conn  -- 递归处理下一条消息

-- 运行服务器
runSocketServer :: IO ()
runSocketServer = withSocketsDo $ do
    putStrLn $ "Starting socket server on port " ++ show port
    bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        bind sock (SockAddrInet port 0)
        listen sock 5  -- 允许最多5个等待连接
        putStrLn "Server is ready to accept connections"
        mainLoop sock
  where
    mainLoop sock = do
        (conn, addr) <- accept sock
        putStrLn $ "Accepted connection from " ++ show addr
        -- 为每个客户端创建一个新线程
        _ <-forkFinally (handleClient conn) (\_ -> close conn)
        mainLoop sock  -- 继续监听新连接
