{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Queries (runTestQuery, runGetCatsQuery, connectDB, getVetsQuery, checkCatExistsQuery, getCatQuery, createAdoptionQuery, getOwnerInfoQuery, getHaskellsQuery, getVetsQueryFull, getAppointmentsQuery, updateCatQuery, deleteVetQuery, createVetQuery) where

import Database.MySQL.Simple
import Data.ByteString
import System.Environment
import Data.ByteString.Char8 as B hiding (unpack)
import Data.Int
import Data.Time (Day, UTCTime)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Aeson as A
import Data.String.Interpolate (i)
import Control.Monad

type Cat = (Int64, ByteString, Double, Int64, Day, Maybe ByteString, Int64, Maybe ByteString, Maybe ByteString)
type Owner = (Int64, ByteString, ByteString, ByteString, ByteString)
type Vet = (Int64, ByteString, ByteString, ByteString, ByteString, Day)
type VetNoID = (ByteString, ByteString, ByteString, ByteString, Day) 
type Appointment = (Int64, Int64, UTCTime, Int64, ByteString, Maybe ByteString)

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

createAppointmentQuery :: ByteString -> IO (Maybe Int64)
createAppointmentQuery jsonData = do
    let entryData = A.decode $ fromStrict jsonData :: Maybe (Int, Int, Day, Int, ByteString, ByteString)
    case entryData of
        Nothing -> return Nothing
        Just (catID, vetID, appointmentDate, durationMinutes, purpose, notes) -> do
            c <- connectDB
            dat <- execute_ c [i|INSERT INTO adoptahaskell.Appointments (catID, veterinarianID, appointmentDate, durationMinutes, appointmentPurpose, appointmentNotes) VALUES (#{catID}, #{vetID}, '2023-07-27 16:43:41', 10, 'appointment purpose goes here', 'appointment notes go here');|]
            return $ Just dat

getCatQuery :: ByteString -> IO ByteString
getCatQuery catid = do
    c <- connectDB
    dat <- query_ c [i|select Cats.*, M.name as medication_name, D2.name as disease_name from Cats left join Prescriptions P on Cats.catID = P.catID left join Medications M on P.medicationID = M.medicationID left join Diagnosis D on Cats.catID = D.catID left join Diseases D2 on D.diseaseID = D2.diseaseID where Cats.catID = #{catid};|] :: IO [Cat]
    return $ toStrict $ A.encode dat


createAdoptionQuery :: ByteString -> IO Bool
createAdoptionQuery jsonObj = do
    let entry = A.decode $ fromStrict jsonObj :: Maybe (ByteString, ByteString, ByteString, ByteString, Int64, Int64, ByteString)
    case entry of
      Nothing -> return False 
      Just (name, address, phone, email, catID, ownerID, adoptionDate) -> do
          c <- connectDB
          dat <- execute_ c [i|start transaction; INSERT INTO adoptahaskell.Owners (name, address, phone, email) VALUES ('#{name}', '#{address}', '#{phone}', '#{email}'); INSERT INTO adoptahaskell.Adoptions (#{catID}, #{ownerID}, #{adoptionDate}) VALUES (3, LAST_INSERT_ID(), '2023-07-27'); commit;|]
          return True

getOwnerInfoQuery :: ByteString -> IO ByteString
getOwnerInfoQuery ownerID = do
    c <- connectDB
    dat <- query_ c [i|select * from Owners where ownerID = #{ownerID};|] :: IO [Owner]
    return $ toStrict $ A.encode dat

getHaskellsQuery :: IO ByteString
getHaskellsQuery = do
    c <- connectDB
    dat <- query_ c [i|select catID, imageURL from Cats;|] :: IO [(ByteString, ByteString)]
    return $ toStrict $ A.encode dat


getVetsQueryFull :: IO ByteString
getVetsQueryFull = do
    c <- connectDB
    dat <- query_ c [i|select * from Veterinarians;|] :: IO [Vet]
    return $ toStrict $ A.encode dat

getAppointmentsQuery :: IO ByteString
getAppointmentsQuery = do
    c <- connectDB
    dat <- query_ c [i| select * from Appointments;|] :: IO [Appointment]
    return $ toStrict $ A.encode dat

updateCatQuery :: ByteString -> ByteString -> IO Bool
updateCatQuery jsonObj catID = do
    let entry = A.decode $ fromStrict jsonObj :: Maybe Cat
    case entry of
      Nothing -> return False
      Just (catID, physicalDescription, weight, age, registrationDate, imageURL, veterinarianID, _, _) -> do
        c <- connectDB
        e <- execute_ c [i|UPDATE adoptahaskell.Cats t SET t.physicalDescription = '#{physicalDescription}' SET t.weight = #{weight} SET t.age = #{age} SET t.registrationDate = '#{registrationDate}' SET t.imageURL = '#{imageURL}' SET t.veterinarianID = #{veterinarianID} WHERE t.catID = #{catID};|]
        return True

deleteVetQuery :: ByteString -> IO ()
deleteVetQuery vetID = do
    c <- connectDB
    r <- execute_ c [i|DELETE FROM adoptahaskell.Veterinarians WHERE veterinarianID = #{vetID};|]
    return ()

createVetQuery :: ByteString -> IO Bool
createVetQuery jsonData = do
    let entry = A.decode $ fromStrict $ jsonData :: Maybe VetNoID
    case entry of
      Nothing -> return False
      Just (name, address, phone, email, hiringDate) -> do
          c <- connectDB
          e <- execute_ c [i|INSERT INTO adoptahaskell.Veterinarians (name, address, phone, email, hiringDate) VALUES ('#{name}', '#{address}', '#{phone}', '#{email}', '#{hiringDate}');|]
          return True

searchVetsQuery :: ByteString -> IO ByteString
searchVetsQuery jsonData = do
    let entry = A.decode $ fromStrict $ jsonData :: Maybe (ByteString, ByteString)
    case entry of
      Nothing -> return ""
      Just (_, val) -> do
          c <- connectDB
          return ""




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
