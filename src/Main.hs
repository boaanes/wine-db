{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Database.SQLite.Simple
import Queries
import Schema
import Web.Scotty

initializeDB :: IO ()
initializeDB = do
  conn <- open "wine.db"
  execute_ conn createBottlesTableQuery
  close conn

insertBottle :: Bottle -> IO ()
insertBottle bottle = do
  conn <- open "wine.db"
  execute
    conn
    insertBottleQuery
    bottle
  close conn

routes :: Connection -> ScottyM ()
routes conn = do
  get "/" $ do
    r <- liftIO $ query_ conn getAllBottlesQuery :: ActionM [Bottle]
    json r
  get "/:id" $ do
    bid <- param "id" :: ActionM Int
    r <- liftIO $ queryNamed conn getBottleByIDQuery [":id" := bid] :: ActionM [Bottle]
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
