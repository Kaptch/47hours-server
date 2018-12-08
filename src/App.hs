{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Logger                 (runStderrLoggingT)
import           Data.String.Conversions              (cs)
import           Data.Text                            (Text)
import           Data.Time
import           Database.Persist.Sqlite              (ConnectionPool,
                                                       Entity (..),
                                                       createSqlitePool,
                                                       entityVal, insert,
                                                       runMigration,
                                                       runSqlPersistMPool,
                                                       runSqlPool, selectFirst,
                                                       selectList, (<=.), (==.))
import           Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger

import           System.IO

import           Servant

import           Api
import           Models
{-
sqlite3 pp.db "CREATE TABLE EmergencyVehicles (vehicleID INTEGER PRIMARY KEY, currLatitude double, currLongitude double, distLatitude double, destLongitude double); INSERT INTO EmergencyVehicles (currLatitude, currLongitude, distLatitude, destLongitude) VALUES (0.0, 0.0, 10.0, 20.0);"
-}

timeFormat = "%H:%M:%S"
understandTime = parseTimeOrError True defaultTimeLocale timeFormat

run :: FilePath -> IO ()
run sqliteFile = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  runSettings settings =<< mkApp sqliteFile

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $
    createSqlitePool (cs sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  return $ logStdoutDev $ app pool

app :: ConnectionPool -> Application
app pool = serve api $ server pool

server :: ConnectionPool -> Server EmergencyVehicleAPI
server pool = vehicleGetAllH :<|> vehicleGetClosestH :<|> vehiclePostInsertH :<|> {-vehiclePostUpdateH :<|>-} test
  where
    vehicleGetAllH = liftIO vehicleGetAll
    vehicleGetAll = flip runSqlPersistMPool pool $ do
      mEmergencyVehicle <- selectList [] []
      return $ entityVal <$> mEmergencyVehicle

    vehicleGetClosestH cLat cLon = case cLat of
        Nothing -> throwError err404
        Just cLa -> case cLon of
          Nothing  -> throwError err404
          Just cLo -> liftIO $ vehicleClosestGet cLa cLo
    vehicleClosestGet :: Double -> Double -> IO [EmergencyVehicle]
    vehicleClosestGet cLat cLon = flip runSqlPersistMPool pool $ do
      mEmergencyVehicle <- selectList [] [] -- TODO
      return $ entityVal <$> mEmergencyVehicle

    vehiclePostInsertH newVehicle = liftIO $ vehiclePostInsert newVehicle
    vehiclePostInsert :: EmergencyVehicle -> IO (Key EmergencyVehicle)
    vehiclePostInsert newVehicle = flip runSqlPersistMPool pool $ insert newVehicle

    test = return [EmergencyVehicle
      { emergencyVehicleCurrLatitude = 0.0,
        emergencyVehicleCurrLongitude = 0.0,
        emergencyVehicleDestLatitude = 11.1,
        emergencyVehicleDestLongitude = 22.2,
        emergencyVehicleUpdated = testTime
      }]

{-
    vehiclePostUpdateH vID newVehicle = liftIO $ vehiclePostUpdate vID newVehicle
    vehiclePostUpdate :: Int -> EmergencyVehicle -> IO ()
    vehiclePostUpdate vID newVehicle = flip runSqlPersistMPool pool $ do
      time <- getCurrentTime
      return $ Just $ update (toPersistKey vID) [emergencyVehicleCurrLatitude =. ]
      -}

testTime :: UTCTime
testTime = understandTime "10:30:20"
