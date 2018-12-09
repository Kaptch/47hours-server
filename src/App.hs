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
import           Data.Int
import           Data.String.Conversions              (cs)
import           Data.Text                            (Text)
import           Data.Time
import           Database.Persist.Sqlite              (ConnectionPool,
                                                       Entity (..),
                                                       createSqlitePool, delete,
                                                       entityVal, insert,
                                                       runMigration,
                                                       runSqlPersistMPool,
                                                       runSqlPool, selectFirst,
                                                       selectList, toSqlKey,
                                                       update, (<.), (<=.),
                                                       (=.), (==.), (>.))

import           Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger

import           System.IO

import           Servant

import           Api
import           Models

{-
sqlite3 "pp.db" "create trigger create_timestamp after insert on emergency_vehicle for each row when (new.updated is null) begin update emergency_vehicle set updated = CURRENT_TIMESTAMP where id = new.id; end;"
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
server pool = vehicleGetClosestH :<|> vehiclePostInsertH :<|> vehiclePostUpdateH :<|> vehicleDeleteH :<|> test
  where
    vehicleGetClosestH cLat cLon = case cLat of
        Nothing -> throwError err406
        Just cLa -> case cLon of
          Nothing  -> throwError err406
          Just cLo -> liftIO $ vehicleClosestGet cLa cLo
    vehicleClosestGet :: Double -> Double -> IO [EmergencyVehicle]
    vehicleClosestGet cLat cLon = flip runSqlPersistMPool pool $ do
      mEmergencyVehicle <- selectList [ EmergencyVehicleCurrLatitude <. cLat + 0.01
                                      , EmergencyVehicleCurrLatitude >. cLat - 0.01
                                      , EmergencyVehicleCurrLongitude <. cLon + 0.01
                                      , EmergencyVehicleCurrLongitude >. cLon - 0.01
                                      ] []
      return $ entityVal <$> mEmergencyVehicle

    vehiclePostInsertH newVehicle = liftIO $ vehiclePostInsert newVehicle
    vehiclePostInsert :: EmergencyVehicle -> IO (Key EmergencyVehicle)
    vehiclePostInsert newVehicle = flip runSqlPersistMPool pool $ insert newVehicle

    vehiclePostUpdateH vID newVehicle = liftIO $ vehiclePostUpdate vID newVehicle
    vehiclePostUpdate :: Int64 -> EmergencyVehicle -> IO ()
    vehiclePostUpdate vID newVehicle = flip runSqlPersistMPool pool $ do
      time <- liftIO getCurrentTime
      update (toSqlKey vID) [
          EmergencyVehicleCurrLatitude =. emergencyVehicleCurrLatitude newVehicle
        , EmergencyVehicleCurrLongitude =. emergencyVehicleCurrLongitude newVehicle
        , EmergencyVehicleDestLatitude =. emergencyVehicleDestLatitude newVehicle
        , EmergencyVehicleDestLongitude =. emergencyVehicleDestLongitude newVehicle
        , EmergencyVehicleUpdated =. Just time
        ]

    vehicleDeleteH vID = liftIO $ vehicleDelete vID
    vehicleDelete :: Int64 -> IO ()
    vehicleDelete vID = flip runSqlPersistMPool pool $ delete (toSqlKey vID :: EmergencyVehicleId)

    test = return [testEmergencyVehicle]

testEmergencyVehicle = EmergencyVehicle
  { emergencyVehicleCurrLatitude = 0.0,
    emergencyVehicleCurrLongitude = 0.0,
    emergencyVehicleDestLatitude = 11.1,
    emergencyVehicleDestLongitude = 22.2,
    emergencyVehicleUpdated = Just testTime
  }

testTime :: UTCTime
testTime = understandTime "10:30:20"
