{-# LANGUAGE OverloadedStrings #-}

module Queries (runTestQuery) where

import Database.MySQL.Simple
import Data.ByteString
import Data.ByteString.Char8 as B
import System.Environment

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



