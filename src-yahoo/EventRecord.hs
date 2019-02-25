{-# LANGUAGE DeriveGeneric #-}

module EventRecord where

import GHC.Generics
import Data.Aeson

data EventRecord = EventRecord
  { user_id :: String
  , page_id :: String
  , ad_id :: String
  , ad_type :: String
  , event_type :: String
  , event_time :: String
  , ip_address :: String
  } deriving (Generic, Show)

instance ToJSON EventRecord where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON EventRecord