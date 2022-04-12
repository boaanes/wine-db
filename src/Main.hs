{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Database.SQLite.Simple
import Schema
import Web.Scotty

initializeDB :: IO ()
initializeDB = do
  conn <- open "wine.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS bottles (id INTEGER PRIMARY KEY, name TEXT, producer TEXT, wineType TEXT, country TEXT, region TEXT, subRegion TEXT, vineyard TEXT, vintage INTEGER, cost INTEGER)"
  close conn

insertBottle :: Bottle -> IO ()
insertBottle bottle = do
  conn <- open "wine.db"
  execute
    conn
    "INSERT INTO bottles (name, producer, wineType, country, region, subRegion, vineyard, vintage, cost) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
    bottle
  close conn

routes :: Connection -> ScottyM ()
routes conn = do
  get "/" $ do
    r <- liftIO $ query_ conn "SELECT name, producer, wineType, country, region, subRegion, vineyard, vintage, cost FROM bottles" :: ActionM [Bottle]
    json r
  get "/:id" $ do
    bid <- param "id" :: ActionM Int
    r <- liftIO $ queryNamed conn "SELECT name, producer, wineType, country, region, subRegion, vineyard, vintage, cost FROM bottles WHERE id = :id" [":id" := bid] :: ActionM [Bottle]
    json r
  post "/" $ do
    bottle <- jsonData :: ActionM Bottle
    liftIO $ insertBottle bottle
    json bottle

main :: IO ()
main = do
  initializeDB
  conn <- open "wine.db"
  scotty 3000 $ routes conn
