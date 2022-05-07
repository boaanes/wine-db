{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Json where

import           Data.Aeson
import           Database
import           GHC.Generics

data FullResponse
    = FullResponse
    { bottle :: Bottle
    , blend  :: [GrapeProportion]
    } deriving (Show, Generic)

instance ToJSON FullResponse where
    toEncoding = genericToEncoding defaultOptions
