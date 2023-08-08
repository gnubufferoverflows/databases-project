{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import System.IO
import Text.Read hiding (lift)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.String.Interpolate

import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import Data.Binary.Builder
import Data.ByteString (ByteString)
import Queries

type Args = Int -- for now

printError :: String -> IO ()
printError = hPutStrLn stderr

response200 :: ByteString -> Response
response200 = responseBuilder status200 [("Content-Type", "text/plain")] . fromByteString 

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

argsParse' :: MaybeT IO Args
argsParse' = do
  args <- lift getArgs
  guard $ length args == 1
  port <- hoistMaybe $ readMaybe $ head args
  guard $ port > 0
  guard $ port < 65535
  hoistMaybe $ Just port

app :: Application
app req respond = do -- the points req and respond will probably be used later
  let path = pathInfo req
  let method = requestMethod req
  if' (path == ["api", "cats"] && method == "GET") $ do
      results <- runGetCatsQuery
      respond $ response200 [i|#{results}|]
  $ do
  results <- runTestQuery 
  respond $ response200 [i|The output is: #{results}|]

main :: IO ()
main = do
  arguments <- runMaybeT argsParse'
  case arguments of
    Just port -> do
      putStrLn [i|Running on port #{port}.|]
      run port app
    Nothing -> printError "Invalid input for port number."

