{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Database where

import           Data.Aeson.Types
import           Data.Int
import qualified Data.Text             as T
import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Sqlite

-- Data constructor for wine type
data WineType
  = Red
  | White
  | Rose
  | Sparkling
  | Other
  deriving (Read, Show, Eq, Ord, Generic, Enum)

instance FromJSON WineType
instance ToJSON WineType where
  toEncoding = genericToEncoding defaultOptions
instance HasSqlValueSyntax be String => HasSqlValueSyntax be WineType where
  sqlValueSyntax = autoSqlValueSyntax
instance FromBackendRow Sqlite WineType where
  fromBackendRow = read . T.unpack <$> fromBackendRow

-- Data constructor for bottle
data BottleT f
  = Bottle
  { _bottleId        :: Columnar f Int32,             -- Primary key
    _bottleName      :: Columnar f T.Text,            -- e.g. Romanée-Conti
    _bottleProducer  :: Columnar f T.Text,            -- e.g. Domaine de la Romanée-Conti
    _bottleWineType  :: Columnar f WineType,          -- e.g. Red
    _bottleCountry   :: Columnar f T.Text,            -- e.g. France
    _bottleRegion    :: Columnar f T.Text,            -- e.g. Burgundy
    _bottleSubRegion :: Columnar f (Maybe T.Text),    -- e.g. Vosne-Romanée
    _bottleVineyard  :: Columnar f (Maybe T.Text),    -- e.g. La Romanée-Conti
    _bottleVintage   :: Columnar f (Maybe Int32),     -- e.g. 1998 - if Nothing, implies it's a NV (or that vintage is unknown)
    _bottleCost      :: Columnar f (Maybe Double)     -- e.g. 35000, price in NOK - if Nothing, cost is unknown (perhaps gift?)
  }
  deriving (Generic)

type Bottle = BottleT Identity
type BottleID = PrimaryKey BottleT Identity
deriving instance Show Bottle
deriving instance Show (PrimaryKey BottleT Identity)
instance Beamable BottleT
instance Table BottleT where
  data PrimaryKey BottleT f = BottleId (Columnar f Int32)
                              deriving (Generic, Beamable)
  primaryKey = BottleId . _bottleId
instance FromJSON Bottle
instance ToJSON Bottle where
  toEncoding = genericToEncoding defaultOptions

newtype WineDB f = WineDB { bottles :: f (TableEntity BottleT) } deriving (Generic)
instance Database be WineDB

wineDB :: DatabaseSettings be WineDB
wineDB = defaultDbSettings
