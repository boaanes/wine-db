{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class
import           Database.SQLite.Simple
import           Queries
import           Schema
import           Web.Scotty

initializeDB :: IO ()
initializeDB = do
  conn <- open "wine.db"
  execute_ conn createBottlesTableQuery
  close conn

insertBottle :: Bottle -> IO ()
insertBottle bottle = do
  conn <- open "wine.db"
  execute conn insertBottleQuery bottle
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
  delete "/:id" $ do
    bid <- param "id" :: ActionM Int
    liftIO $ executeNamed conn deleteBottleByIDQuery [":id" := bid]
    json ()
  put "/:id" $ do
    bid <- param "id" :: ActionM Int
    bottle <- jsonData :: ActionM Bottle
    liftIO $
      executeNamed conn updateBottleByIDQuery
        [ ":id" := bid
        , ":name" := name bottle
        , ":producer" := producer bottle
        , ":wineType" := wineType bottle
        , ":country" := country bottle
        , ":region" := region bottle
        , ":subRegion" := subRegion bottle
        , ":vineyard" := vineyard bottle
        , ":vintage" := vintage bottle
        , ":cost" := cost bottle
        ]
    json bottle

main :: IO ()
main = do
  initializeDB
  conn <- open "wine.db"
  scotty 3000 $ routes conn
