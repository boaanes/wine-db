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
    { bottleId :: Int                   -- autogenerate?
    , name :: String                    -- e.g. Romanée-Conti
    , producer :: String                -- e.g. Domaine de la Romanée-Conti
    , wineType :: WineType              -- e.g. Red
    , country :: String                 -- e.g. France
    , region :: String                  -- e.g. Burgundy
    , subRegion :: Maybe String         -- e.g. Vosne-Romanée
    , vineyard :: Maybe String          -- e.g. La Romanée-Conti
    , vintage :: Maybe Int              -- e.g. 1998 - if Nothing, implies it's a NV
    , cost :: Maybe Int                 -- e.g. 35000, price in NOK - if Nothing, cost is unknown (perhaps gift?)
    , blend :: Maybe [(String, Int)]    -- e.g. [("Pinot Noir", 100)] or [("Montepulciano", 85), ("Merlot", 15)] list of grapes with percentage
    }
    deriving Generic

instance FromJSON Bottle
instance ToJSON Bottle where
    toEncoding = genericToEncoding defaultOptions

getBottle :: Int -> Bottle
getBottle bottleId = (Bottle bottleId "Quaranta" "Pennessi" Red "Italy" "Tuscany" (Just "Montepulciano") Nothing (Just 2020) (Just 190) (Just [("Montepulciano", 85), ("Merlot", 15)]))

main :: IO ()
main = scotty 3000 $ do
  get "/bottle/:bottleId" $ do
    rawId <- param "bottleId"
    let bottleId = read rawId :: Int
    json $ getBottle bottleId
  post "/bottle" $ do
    parsedJson <- jsonData :: ActionM Bottle
    json parsedJson
