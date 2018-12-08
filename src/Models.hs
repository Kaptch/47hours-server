{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import Data.Aeson
import Data.Text
import Data.Time
import Data.Maybe

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
EmergencyVehicle
  currLatitude    Double
  currLongitude   Double
  destLatitude    Double
  destLongitude   Double
  updated         UTCTime Maybe default=CURRENT_TIME
  deriving Eq Read Show
|]

instance FromJSON EmergencyVehicle where
  parseJSON = withObject "EmergencyVehicle" $ \ v ->
    EmergencyVehicle <$> v .: "currLatitude"
         <*> v .: "currLongitude"
         <*> v .: "destLatitude"
         <*> v .: "destLongitude"
         <*> v .:? "updated"

instance ToJSON EmergencyVehicle where
  toJSON (EmergencyVehicle cLat cLon dLat dLon t) =
    object ["currLatitude" .= cLat
      , "currLongitude" .= cLon
      , "destLatitude" .= dLat
      , "destLongitude" .= dLon
      , "updated" .= t
          ]
