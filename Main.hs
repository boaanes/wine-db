{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty

import GHC.Generics
import Data.Aeson.Types


-- Data constructor for wine type
data WineType
    = Red
    | White
    | Sparkling
    deriving Generic

instance FromJSON WineType
instance ToJSON WineType where
    toEncoding = genericToEncoding defaultOptions


-- Data constructor for bottle
data Bottle = Bottle
    { bottleId :: Int           -- autogenerate?
    , name :: String
    , producer :: String
    , wineType :: WineType
    , vintage :: Maybe Int      -- if Nothing, implies it's a NV
    , cost :: Maybe Int         -- if Nothing, cost is unknown (perhaps gift?)
    }
    deriving Generic

instance FromJSON Bottle
instance ToJSON Bottle where
    toEncoding = genericToEncoding defaultOptions

getBottle :: Int -> Bottle
getBottle bottleId = (Bottle bottleId "Quaranta" "Pennessi" Red (Just 2020) (Just 190))

main :: IO ()
main = scotty 3000 $ do
  get "/bottle/:bottleId" $ do
    rawId <- param "bottleId"
    let bottleId = read rawId :: Int
    json $ getBottle bottleId
  post "/bottle" $ do
    parsedJson <- jsonData :: ActionM Bottle
    json parsedJson
