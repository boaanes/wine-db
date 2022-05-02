module Routes where

import           Control.Monad.IO.Class
import           Data.Aeson                (decode)
import           Database
import           Database.SQLite.Simple
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status
import           Polet
import           Queries
import           Web.Scotty

routes :: Connection -> ScottyM ()
routes conn = do
  get "/" $ do
    bs <- liftIO $ getBottles conn
    json bs
  get "/:id" $ do
    bid <- param "id" :: ActionM Int
    b <- liftIO $ getBottleByID conn (fromIntegral bid)
    case b of
      Just bot -> json bot
      Nothing  -> status notFound404 >> raw "Bottle not found"
  post "/" $ do
    bottle <- jsonData :: ActionM Bottle
    liftIO $ postBottle conn bottle
    json bottle
  post "/:id" $ do
    bid <- param "id" :: ActionM String
    poletResponse <- liftIO $ httpLbs $ parseRequest_ $ "http://www.vinmonopolet.no/api/products/" ++ bid ++ "?fields=code,name,price,year,main_category,main_country,main_producer,district,sub_District"
    case decode (getResponseBody poletResponse) :: Maybe PoletResponse of
      Just poletBottle -> json $ fromPoletResponseToBottle poletBottle
      Nothing          -> status notFound404 >> raw "Polet bottle not found"
  delete "/:id" $ do
    bid <- param "id" :: ActionM Int
    liftIO $ deleteBottleByID conn (fromIntegral bid)
    json ()
  put "/" $ do
    bottle <- jsonData :: ActionM Bottle
    liftIO $ updateBottle conn bottle
    json bottle