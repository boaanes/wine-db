{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class
import           Database.SQLite.Simple
import           Network.HTTP.Types.Status
import           Queries
import           Schema
import           Web.Scotty

initializeDB :: IO ()
initializeDB = do
  conn <- open "wine.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS bottles (id INTEGER PRIMARY KEY, name TEXT, producer TEXT, wine_type TEXT, country TEXT, region TEXT, sub_region TEXT, vineyard TEXT, vintage INTEGER, cost INTEGER)"
  close conn

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
  delete "/:id" $ do
    bid <- param "id" :: ActionM Int
    liftIO $ deleteBottleByID conn (fromIntegral bid)
    json ()
  put "/" $ do
    bottle <- jsonData :: ActionM Bottle
    liftIO $ updateBottle conn bottle
    json bottle

main :: IO ()
main = do
  initializeDB
  conn <- open "wine.db"
  scotty 3000 $ routes conn
