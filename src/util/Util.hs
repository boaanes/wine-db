module Util where

import           Data.Char (toLower, toUpper)
import qualified Data.Text as T

-- Uppercase first letter in each word
capitalizeFirst :: T.Text -> T.Text
capitalizeFirst = T.pack . unwords . map (\x -> toUpper (head x) : map toLower (tail x)) . words . T.unpack
