module Main where

import Servant (serve)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Interpreter

app :: Application
app = serve apiProxy handlers

main :: IO ()
main = do
  putStrLn "Server running on localhost:3000"
  run 3000 $ logStdoutDev app
