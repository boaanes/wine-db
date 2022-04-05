{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty

import Control.Exception.Base

import GHC.Generics
import Data.Aeson.Types
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

import qualified Data.Text as T
import qualified Data.Int as I

-- Data constructor for wine type
data WineType
    = Red
    | White
    | Sparkling
    deriving (Show, Eq, Ord, Generic)

instance FromJSON WineType
instance ToJSON WineType where
    toEncoding = genericToEncoding defaultOptions
instance FromField WineType where
    fromField field = case (fieldData field) of
            (SQLText "Red")         -> return Red
            (SQLText "White")       -> return White
            (SQLText "Sparkling")   -> return Red
            (SQLText other)         -> returnError Incompatible field ("Illegal value: '" ++ (T.unpack other) ++ "'")
instance ToField WineType where
    toField wineType = case wineType of
            Red         -> (SQLText "Red")
            White       -> (SQLText "White")
            Sparkling   -> (SQLText "Sparkling")

-- Data constructor for bottle
data Bottle = Bottle
    { name :: String                    -- e.g. Romanée-Conti
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
    fromRow = Bottle <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
instance ToRow Bottle where
    toRow (Bottle name producer wineType country region (Just subRegion) (Just vineyard) (Just vintage) (Just cost)) =
        [(SQLText (T.pack name)), (SQLText (T.pack producer)), (toField wineType), (SQLText (T.pack country)), (SQLText (T.pack region)), (SQLText (T.pack subRegion)), (SQLText (T.pack vineyard)), (SQLInteger (fromIntegral vintage)), (SQLInteger (fromIntegral cost))]

getBottle :: Int -> Bottle
getBottle bottleId = (Bottle "Quaranta" "Pennessi" Red "Italy" "Tuscany" (Just "Montepulciano") Nothing (Just 2020) (Just 190))

{-
main :: IO ()
main = do
    conn <- open "wine.db"
    r <- query_ conn "SELECT * FROM bottles" :: IO [Bottle]
    mapM_ print r
    close conn
-}

insertBottle :: Bottle -> IO ()
insertBottle bottle = do
    conn <- open "wine.db"
    execute conn "INSERT INTO bottles (name, producer, wineType, country, region, subRegion, vineyard, vintage, cost) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)" bottle
    close conn

getBottleById :: Int -> IO ()
getBottleById id = do
    conn <- open "wine.db"
    r <- queryNamed conn "SELECT * FROM bottles WHERE id = :id" [":id" := (id :: Int)]
    mapM_ (mapM_ putStrLn) (r :: [[String]])
    close conn

main :: IO ()
main = scotty 3000 $ do
  get "/bottle/:bottleId" $ do
    rawId <- param "bottleId"
    let bottleId = read rawId :: Int
    json $ getBottle bottleId
  post "/bottle" $ do
    parsedJson <- jsonData :: ActionM Bottle
    json parsedJson

