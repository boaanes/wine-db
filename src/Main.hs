{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Database.SQLite.Simple
import Schema
import Web.Scotty

getBottle :: Int -> Bottle
getBottle _ =
  Bottle
    "Quaranta"
    "Pennessi"
    Red
    "Italy"
    "Tuscany"
    (Just "Montepulciano")
    Nothing
    (Just 2020)
    (Just 190)

{-
main :: IO ()
main = do
    conn <- open "wine.db"
    r <- query_ conn "SELECT * FROM bottles" :: IO [Bottle]
    mapM_ print r
    close conn
-}

insertBottle :: Bottle -> IO ()
insertBottle bottle = do
  conn <- open "wine.db"
  execute
    conn
    "INSERT INTO bottles (name, producer, wineType, country, region, subRegion, vineyard, vintage, cost) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
    bottle
  close conn

getBottleById :: Int -> IO ()
getBottleById id = do
  conn <- open "wine.db"
  r <-
    queryNamed
      conn
      "SELECT * FROM bottles WHERE id = :id"
      [":id" := (id :: Int)]
  mapM_ (mapM_ putStrLn) (r :: [[String]])
  close conn

main :: IO ()
main = scotty 3000 $
  do
    get "/bottle/:bottleId" $
      do
        rawId <- param "bottleId"
        let bottleId = read rawId :: Int
        json $ getBottle bottleId
    post "/bottle" $
      do
        parsedJson <- jsonData :: ActionM Bottle
        json parsedJson
