{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module PoletAPI where

import           Data.Aeson.Types
import           Data.Int
import qualified Data.Text        as T
import           GHC.Generics
import           Schema

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

data PoletResponse
  = PoletResponse
  { code          :: T.Text
  , name          :: T.Text
  , main_category :: MainCategory
  , main_country  :: MainCountry
  , main_producer :: MainProducer
  , district      :: District
  } deriving (Generic, FromJSON, Show)

instance ToJSON PoletResponse where
  toEncoding = genericToEncoding defaultOptions

fromPoletResponseToBottle :: PoletResponse -> Bottle
fromPoletResponseToBottle (PoletResponse c n (MainCategory mc) (MainCountry mc2) (MainProducer mp) (District d)) =
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
  , _bottleSubRegion = Nothing
  , _bottleVineyard = Nothing
  , _bottleVintage = Nothing
  , _bottleCost = Nothing
  }
