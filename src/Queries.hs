{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleInstances #-}

module Queries where

import           Data.Int               (Int32)
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.SQLite.Simple
import           Schema

getBottleByID' :: Int32 -> SqliteM (Maybe Bottle)
getBottleByID' bid =
  runSelectReturningOne $
  select $
  filter_ (\b -> _bottleId b ==. val_ bid) $
  all_ (bottles wineDB)

getBottleByID :: Connection -> Int32 -> IO (Maybe Bottle)
getBottleByID conn bid =
  runBeamSqlite conn $
  getBottleByID' bid

postBottle' :: Bottle -> SqliteM ()
postBottle' bottle =
  runInsert $
  insert (bottles wineDB) $
  insertExpressions [ Bottle
                     { _bottleId = default_ -- handles autoincrement
                     , _bottleName = val_ (_bottleName bottle)
                     , _bottleProducer = val_ (_bottleProducer bottle)
                     , _bottleWineType = val_ (_bottleWineType bottle)
                     , _bottleCountry = val_ (_bottleCountry bottle)
                     , _bottleRegion = val_ (_bottleRegion bottle)
                     , _bottleSubRegion = val_ (_bottleSubRegion bottle)
                     , _bottleVineyard = val_ (_bottleVineyard bottle)
                     , _bottleVintage = val_ (_bottleVintage bottle)
                     , _bottleCost = val_ (_bottleCost bottle)
                     }
                   ]

postBottle :: Connection -> Bottle -> IO ()
postBottle conn bottle =
  runBeamSqlite conn $
  postBottle' bottle

deleteBottleByID' :: Int32 -> SqliteM ()
deleteBottleByID' bid =
  runDelete $
    delete (bottles wineDB)
    (\b -> _bottleId b ==. val_ bid)

deleteBottleByID :: Connection -> Int32 -> IO ()
deleteBottleByID conn bid =
  runBeamSqlite conn $
  deleteBottleByID' bid

updateBottle' :: Bottle -> SqliteM ()
updateBottle' bottle =
  runUpdate $
    save (bottles wineDB) bottle

updateBottle :: Connection -> Bottle -> IO ()
updateBottle conn bottle =
  runBeamSqlite conn $
  updateBottle' bottle

getBottles' :: SqliteM [Bottle]
getBottles' =
  runSelectReturningList $
  select $
  all_ (bottles wineDB)

getBottles :: Connection -> IO [Bottle]
getBottles conn =
  runBeamSqlite conn
  getBottles'
