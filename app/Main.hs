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
import Web.Scotty as S
import Network.Wai.Middleware.Static

type Args = Int -- for now

printError :: String -> IO ()
printError = hPutStrLn stderr

response200 :: ByteString -> Response
response200 = responseBuilder status200 [("Content-Type", "text/plain")] . fromByteString 

argsParse' :: MaybeT IO Args
argsParse' = do
  args <- lift getArgs
  guard $ length args == 1
  port <- hoistMaybe $ readMaybe $ head args
  guard $ port > 0
  guard $ port < 65535
  hoistMaybe $ Just port

app' port = scotty port $ do
    S.middleware $ staticPolicy (noDots >-> addBase "website/")
    S.get "/" $ file "website/index.html"
    S.get "/api/cats" $ do
        results <- lift runGetCatsQuery
        S.json $ results
    S.get "/api/vets/partial" $ do
        results <- lift getVetsQuery
        S.json $ results
    S.get "/api/cats/exists/:id" $ do
        catid <- param "id"
        results <- lift $ checkCatExistsQuery catid
        S.json $ results

main :: IO ()
main = do
  arguments <- runMaybeT argsParse'
  case arguments of
    Just port -> do
      putStrLn [i|Running on port #{port}.|]
      app' port
    Nothing -> printError "Invalid input for port number."

