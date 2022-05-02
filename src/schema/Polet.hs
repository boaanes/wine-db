{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Polet where

import           Data.Aeson.Types
import           Data.Int
import qualified Data.Text        as T
import           Database
import           GHC.Generics

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
  { countryCode :: T.Text
  } deriving (Generic, Show)

instance FromJSON MainCountry where
  parseJSON = withObject "MainCountry" $ \o ->
    MainCountry <$> o .: "code"
instance ToJSON MainCountry where
  toJSON (MainCountry c) = object ["code" .= c]

newtype MainProducer
  = MainProducer
  { producerCode :: T.Text
  } deriving (Generic, Show)

instance FromJSON MainProducer where
  parseJSON = withObject "MainProducer" $ \o ->
    MainProducer <$> o .: "code"
instance ToJSON MainProducer where
  toJSON (MainProducer c) = object ["code" .= c]

newtype District
  = District
  { districtCode :: T.Text
  } deriving (Generic, Show)

instance FromJSON District where
  parseJSON = withObject "District" $ \o ->
    District <$> o .: "code"
instance ToJSON District where
  toJSON (District c) = object ["code" .= c]

newtype SubDistrict
  = SubDistrict
  { subDistrictCode :: T.Text
  } deriving (Generic, Show)

instance FromJSON SubDistrict where
  parseJSON = withObject "SubDistrict" $ \o ->
    SubDistrict <$> o .: "code"
instance ToJSON SubDistrict where
  toJSON (SubDistrict c) = object ["code" .= c]

newtype Price
  = Price
  { value :: Double
  } deriving (Generic, Show)

instance FromJSON Price where
  parseJSON = withObject "Price" $ \o ->
    Price <$> o .: "value"
instance ToJSON Price where
  toJSON (Price v) = object ["value" .= v]

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
  } deriving (Generic, FromJSON, Show)

instance ToJSON PoletResponse where
  toEncoding = genericToEncoding defaultOptions

fromPoletResponseToBottle :: PoletResponse -> Bottle
fromPoletResponseToBottle (PoletResponse c n (Price v) y (MainCategory mc) (MainCountry mc2) (MainProducer mp) (District d) sd) =
  Bottle
  { _bottleId = read (T.unpack c) :: Int32
  , _bottleName = n
  , _bottleProducer = mp
  , _bottleWineType = case mc of
    "rødvin"         -> Red
    "hvitvin"        -> White
    "rosévin"        -> Rose
    "musserende_vin" -> Sparkling
    _                -> Other
  , _bottleCountry = mc2
  , _bottleRegion = d
  , _bottleSubRegion = case sd of
    Just (SubDistrict s) -> Just s
    Nothing              -> Nothing
  , _bottleVineyard = Nothing
  , _bottleVintage = Just (read (T.unpack y) :: Int32)
  , _bottleCost = Just v
  }
