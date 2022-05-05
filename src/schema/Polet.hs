{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Polet where

import           Data.Aeson.Types
import           Data.Int
import qualified Data.Text            as T
import           Database
import           Database.Beam
import           Database.Beam.Sqlite

newtype MainCategory
  = MainCategory
  { categoryCode :: T.Text
  } deriving (Generic, Show)

instance FromJSON MainCategory where
  parseJSON = withObject "MainCategory" $ \o ->
    MainCategory <$> o .: "code"
instance ToJSON MainCategory where
  toJSON (MainCategory c) = object ["code" .= c]

newtype MainCountry
  = MainCountry
  { countryName :: T.Text
  } deriving (Generic, Show)

instance FromJSON MainCountry where
  parseJSON = withObject "MainCountry" $ \o ->
    MainCountry <$> o .: "name"
instance ToJSON MainCountry where
  toJSON (MainCountry c) = object ["name" .= c]

newtype MainProducer
  = MainProducer
  { producerName :: T.Text
  } deriving (Generic, Show)

instance FromJSON MainProducer where
  parseJSON = withObject "MainProducer" $ \o ->
    MainProducer <$> o .: "name"
instance ToJSON MainProducer where
  toJSON (MainProducer c) = object ["name" .= c]

newtype District
  = District
  { districtName :: T.Text
  } deriving (Generic, Show)

instance FromJSON District where
  parseJSON = withObject "District" $ \o ->
    District <$> o .: "name"
instance ToJSON District where
  toJSON (District c) = object ["name" .= c]

newtype SubDistrict
  = SubDistrict
  { subDistrictName :: T.Text
  } deriving (Generic, Show)

instance FromJSON SubDistrict where
  parseJSON = withObject "SubDistrict" $ \o ->
    SubDistrict <$> o .: "name"
instance ToJSON SubDistrict where
  toJSON (SubDistrict c) = object ["name" .= c]

newtype Price
  = Price
  { value :: Double
  } deriving (Generic, Show)

instance FromJSON Price where
  parseJSON = withObject "Price" $ \o ->
    Price <$> o .: "value"
instance ToJSON Price where
  toJSON (Price v) = object ["value" .= v]

data Raastoff
  = Raastoff
  { raastaffName       :: T.Text
  , raastaffPercentage :: T.Text
  } deriving (Generic, Show)

instance FromJSON Raastoff where
  parseJSON = withObject "Raastoff" $ \o ->
    Raastoff <$> o .: "name"
             <*> o .: "percentage"
instance ToJSON Raastoff where
  toJSON (Raastoff n p) = object ["name" .= n, "percentage" .= p]

data PoletResponse
  = PoletResponse
  { code          :: T.Text
  , name          :: T.Text
  , price         :: Price
  , year          :: T.Text
  , main_category :: MainCategory
  , main_country  :: MainCountry
  , main_producer :: MainProducer
  , district      :: District
  , sub_District  :: Maybe SubDistrict
  , raastoff      :: Maybe [Raastoff]
  } deriving (Generic, FromJSON, Show)

instance ToJSON PoletResponse where
  toEncoding = genericToEncoding defaultOptions

grapeHelper :: GrapeProportion -> GrapeProportionT (QExpr Sqlite s')
grapeHelper gp =
  GrapeProportion
  { _grapeproportionId = default_
  , _grapeproportionName = val_ $ _grapeproportionName gp
  , _grapeproportionPercentage = val_ $ _grapeproportionPercentage gp
  , _grapeproportionBottle = val_ $ _grapeproportionBottle gp
  }

toGrape :: Bottle -> Raastoff -> GrapeProportion
toGrape b (Raastoff n p) =
  GrapeProportion
  { _grapeproportionId = -1
  , _grapeproportionName = n
  , _grapeproportionPercentage = read (T.unpack p) :: Double
  , _grapeproportionBottle = primaryKey b
  }

fromPoletResponseToGrapeProportions :: PoletResponse -> Bottle -> [GrapeProportion]
fromPoletResponseToGrapeProportions pr bottle =
  case raastoff pr of
    Just r  -> map (toGrape bottle) r
    Nothing -> []

fromPoletResponseToBottle :: PoletResponse -> Bottle
fromPoletResponseToBottle pr =
  Bottle
  { _bottleId = -1
  , _bottlePoletId = Just (read (T.unpack (code pr)) :: Int32)
  , _bottleName = name pr
  , _bottleProducer = producerName (main_producer pr)
  , _bottleWineType = case categoryCode (main_category pr) of
                        "rødvin"         -> Red
                        "hvitvin"        -> White
                        "rosévin"        -> Rose
                        "musserende_vin" -> Sparkling
                        _                -> Other
  , _bottleCountry = countryName $ main_country pr
  , _bottleDistrict = districtName $ district pr
  , _bottleSubDistrict = case sub_District pr of
                           Just (SubDistrict s) -> Just s
                           Nothing              -> Nothing
  , _bottleVineyard = Nothing
  , _bottleVintage = Just (read (T.unpack (year pr)) :: Int32)
  , _bottleCost = Just $ value $ price pr
  }
