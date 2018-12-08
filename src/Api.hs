{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Data.Proxy
import Data.Text
import Data.Int

import Database.Persist

import Models

import Servant.API

type EmergencyVehicleAPI =
  "vehicle" :> Get '[JSON] [EmergencyVehicle] :<|>
  "vehicle" :> QueryParam "latitude" Double :> QueryParam "longitude" Double :> Get '[JSON] [EmergencyVehicle] :<|>
  "vehicle" :> ReqBody '[JSON] EmergencyVehicle :> Post '[JSON] (Key EmergencyVehicle) :<|>
  "vehicle" :> Capture "key" Int64 :> ReqBody '[JSON] EmergencyVehicle :> Put '[JSON] () :<|>
  "vehicle" :> Capture "key" Int64 :> Delete '[JSON] () :<|>
  "test" :> Get '[JSON] [EmergencyVehicle]

api :: Proxy EmergencyVehicleAPI
api = Proxy
