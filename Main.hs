{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty

import GHC.Generics
import Data.Aeson.Types
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField


-- Data constructor for wine type
data WineType
    = Red
    | White
    | Sparkling
    deriving (Show, Eq, Generic)

instance FromJSON WineType
instance ToJSON WineType where
    toEncoding = genericToEncoding defaultOptions
instance FromField WineType where
    fromField f mdata =
        return wineType
        where wineType =
            case mdata of
                (Just 0) -> Red
                (Just 1) -> White
                _      -> Sparkling


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
    }
    deriving (Show, Eq, Generic)

instance FromJSON Bottle
instance ToJSON Bottle where
    toEncoding = genericToEncoding defaultOptions
instance FromRow Bottle where
    fromRow = Bottle <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

getBottle :: Int -> Bottle
getBottle bottleId = (Bottle bottleId "Quaranta" "Pennessi" Red "Italy" "Tuscany" (Just "Montepulciano") Nothing (Just 2020) (Just 190))

main :: IO ()
main = scotty 3000 $ do
  get "/bottle/:bottleId" $ do
    rawId <- param "bottleId"
    let bottleId = read rawId :: Int
    json $ getBottle bottleId
  post "/bottle" $ do
    parsedJson <- jsonData :: ActionM Bottle
    json parsedJson
