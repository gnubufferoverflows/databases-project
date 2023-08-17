{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Queries (runTestQuery, runGetCatsQuery, connectDB, getVetsQuery, checkCatExistsQuery) where

import Database.MySQL.Simple
import Data.ByteString
import System.Environment
import Data.ByteString.Char8 as B hiding (unpack)
import Data.Int
import Data.Time (Day)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Aeson as A
import Data.String.Interpolate (i)
import Control.Monad

type Cat = (Int64, ByteString, Double, Int64, Day, Maybe ByteString, Int64, Maybe ByteString, Maybe ByteString)

instance ToJSON ByteString where
    toJSON = toJSON . decodeUtf8

instance FromJSON ByteString where
    parseJSON (String str) = pure . encodeUtf8 $ str
    parseJSON _ = fail "this message should NOT APPEAR"  

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

getVetsQuery :: IO ByteString
getVetsQuery = do
    c <- connectDB
    dat <- query_ c "select veterinarianID, name from adoptahaskell.Veterinarians;" :: IO [(Int, ByteString)]
    return $ toStrict $ A.encode $ dat

checkCatExistsQuery :: ByteString -> IO ByteString
checkCatExistsQuery catid = do
    c <- connectDB
    dat <- query_ c [i|select exists(select * from adoptahaskell.Cats where catID = #{catid});|] :: IO [(ByteString, Int)] 
    return $ toStrict $ A.encode dat

createAppointmentQuery :: ByteString -> IO Int64
createAppointmentQuery json = do
    let appointment = A.decode $ fromStrict json :: Maybe (Int, Int, Day, Int, ByteString, ByteString)
    c <- connectDB
    execute_ c [i|INSERT INTO adoptahaskell.Appointments (catID, veterinarianID, appointmentDate, durationMinutes, appointmentPurpose, appointmentNotes) VALUES (3, 2, '2023-07-27 16:43:41', 10, 'appointment purpose goes here', 'appointment notes go here');|] 


runTestQuery :: IO ByteString
runTestQuery = do
  c <- connectDB
  execute_ c "DROP TABLE IF EXISTS diagnostic" -- we don't really care what these queries output
  execute_ c "CREATE TABLE diagnostic(id INT PRIMARY KEY, text VARCHAR(255) NOT NULL)"
  execute_ c "INSERT INTO diagnostic (id, text) VALUES (1, 'MySQL is working')"
  rows <- query_ c "SELECT * FROM diagnostic" :: IO [(ByteString, Int)]
  return $ B.pack $ show rows 




runGetCatsQuery :: IO ByteString
runGetCatsQuery = do
    c <- connectDB
    cats <- query_ c "select Cats.*, M.name as medication_name, D2.name as disease_name from Cats left join Prescriptions P on Cats.catID = P.catID left join Medications M on P.medicationID = M.medicationID left join Diagnosis D on Cats.catID = D.catID left join Diseases D2 on D.diseaseID = D2.diseaseID;" :: IO [Cat]
    return $ toStrict $ A.encode cats
