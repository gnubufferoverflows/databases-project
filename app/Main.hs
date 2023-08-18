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
import Network.HTTP.Types (status200, status400, ok200)
import Network.Wai.Handler.Warp (run)
import Data.Binary.Builder
import Data.ByteString (ByteString, fromStrict, toStrict)
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
    S.get "/api/cats/:id" $ do
        catid <- param "id"
        results <- lift $ getCatQuery catid
        S.json $ results
    S.post "/api/adoption" $ do
        b <- body
        results <- lift $ createAdoptionQuery $ toStrict b
        case results of
          True -> S.status ok200
          False -> do
              S.status status400
              S.text "JSON could not be parsed."
    S.get "/api/owners/:id" $ do
        ownerID <- param "id"
        results <- lift $ getOwnerInfoQuery ownerID
        S.json $ results
    S.get "/api/haskells" $ do
        results <- lift $ getHaskellsQuery
        S.json $ results
    S.get "/api/vets" $ do
        results <- lift $ getVetsQueryFull
        S.json $ results
    S.get "/api/appointments" $ do
        results <- lift $ getAppointmentsQuery
        S.json $ results
    S.patch "/api/cats/:id" $ do
        b <- body
        catID <- param "id"
        results <- lift $ updateCatQuery (toStrict b) catID
        case results of
          True -> S.status ok200
          False -> do
              S.status status400
              S.text "Invalid JSON"
    S.delete "/api/vets/:id" $ do
        catID <- param "id"
        results <- lift $ deleteVetQuery catID
        S.status ok200
    S.post "/api/vets" $ do
        b <- body
        results <- lift $ createVetQuery $ toStrict b
        case results of
          True -> S.status ok200
          False -> do
              S.status status400
              S.text "bad json"




main :: IO ()
main = do
  arguments <- runMaybeT argsParse'
  case arguments of
    Just port -> do
      putStrLn [i|Running on port #{port}.|]
      app' port
    Nothing -> printError "Invalid input for port number."

