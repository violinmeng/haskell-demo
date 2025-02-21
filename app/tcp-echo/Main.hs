module Main (main) where

import SocketServer (runSocketServer)
import SocketClient (runSocketClient)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import System.IO

main :: IO ()
main = do
    -- 在新线程中启动服务器
    forkIO runSocketServer

    -- 主线程运行客户端交互
    putStrLn "Socket server started. Enter text to send (or 'quit' to exit):"
    forever $ do
        putStr "> "
        hFlush stdout  -- 确保提示符立即显示
        input <- getLine
        if input == "quit"
            then return ()
            else do
                response <- runSocketClient input
                putStrLn $ "Server responded with: " ++ response
