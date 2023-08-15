{-# LANGUAGE OverloadedStrings #-}

module Queries (runTestQuery, runGetCatsQuery) where

import Database.MySQL.Simple
import Data.ByteString
import Data.ByteString.Char8 as B
import System.Environment
import Data.ByteString.Base64 as Base64
import Data.Text.Encoding
import Data.Aeson (Value)
import Data.Int
import Data.Time (Day)
import Data.Aeson

type Cat = (Int64, ByteString, Double, Int64, Day, Maybe ByteString, Int64, Maybe ByteString, Maybe ByteString)

instance ToJSON ByteString where
    toJSON bs = String . decodeUtf8 . Base64.encode $ bs

connectDB :: IO Connection
connectDB = do
  host <- getEnv "DB_HOST"
  user <- getEnv "DB_USER"
  passwd <- getEnv "DB_PASSWORD"
  database <- getEnv "DB_DATABASE"
  connect defaultConnectInfo {
                                connectHost = host,
                                connectUser = user,
                                connectPassword = passwd,
                                connectDatabase = database
                             }

runTestQuery :: IO ByteString
runTestQuery = do
  c <- connectDB
  execute_ c "DROP TABLE IF EXISTS diagnostic" -- we don't really care what these queries output
  execute_ c "CREATE TABLE diagnostic(id INT PRIMARY KEY, text VARCHAR(255) NOT NULL)"
  execute_ c "INSERT INTO diagnostic (id, text) VALUES (1, 'MySQL is working')"
  rows <- query_ c "SELECT * FROM diagnostic" :: IO [(Int, String)]
  return $ B.pack $ show rows 


runGetCatsQuery :: IO ByteString
runGetCatsQuery = do
    c <- connectDB
    cats <- query_ c "select Cats.*, M.name as medication_name, D2.name as disease_name from Cats left join Prescriptions P on Cats.catID = P.catID left join Medications M on P.medicationID = M.medicationID left join Diagnosis D on Cats.catID = D.catID left join Diseases D2 on D.diseaseID = D2.diseaseID;" :: IO [Cat]
    return $ B.pack $ show cats
