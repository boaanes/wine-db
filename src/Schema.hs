{-# LANGUAGE DeriveGeneric #-}

module Schema where

import           Data.Aeson.Types
import qualified Data.Text                        as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField   (ToField (..))
import           GHC.Generics

-- Data constructor for wine type
data WineType
  = Red
  | White
  | Rose
  | Sparkling
  deriving (Show, Eq, Ord, Generic)

instance FromJSON WineType

instance ToJSON WineType where
  toEncoding = genericToEncoding defaultOptions

instance FromField WineType where
  fromField f = case fieldData f of
    (SQLText "Red") -> return Red
    (SQLText "White") -> return White
    (SQLText "Rose") -> return Rose
    (SQLText "Sparkling") -> return Red
    (SQLText other) ->
      returnError
        Incompatible
        f
        ("Illegal value: '" ++ T.unpack other ++ "'")
    _ -> returnError Incompatible f "Could not parse WineType"

instance ToField WineType where
  toField Red       = SQLText "Red"
  toField White     = SQLText "White"
  toField Rose      = SQLText "Rose"
  toField Sparkling = SQLText "Sparkling"

-- Data constructor for bottle
data Bottle = Bottle
  { name      :: String,        -- e.g. Romanée-Conti
    producer  :: String,        -- e.g. Domaine de la Romanée-Conti
    wineType  :: WineType,      -- e.g. Red
    country   :: String,        -- e.g. France
    region    :: String,        -- e.g. Burgundy
    subRegion :: Maybe String,  -- e.g. Vosne-Romanée
    vineyard  :: Maybe String,  -- e.g. La Romanée-Conti
    vintage   :: Maybe Int,     -- e.g. 1998 - if Nothing, implies it's a NV (or that vintage is unknown)
    cost      :: Maybe Int      -- e.g. 35000, price in NOK - if Nothing, cost is unknown (perhaps gift?)
  }
  deriving (Show, Eq, Generic)

instance FromJSON Bottle

instance ToJSON Bottle where
  toEncoding = genericToEncoding defaultOptions

instance FromRow Bottle where
  fromRow = Bottle <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Bottle where
  toRow b =
    [ toField (name b),
      toField (producer b),
      toField (wineType b),
      toField (country b),
      toField (region b),
      toField (subRegion b),
      toField (vineyard b),
      toField (vintage b),
      toField (cost b)
    ]
