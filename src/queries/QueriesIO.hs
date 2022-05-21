module QueriesIO where

import           Data.Int                                 (Int32)
import           Database
import           Database.Beam
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Sqlite
import           Database.SQLite.Simple
import           Polet                                    (grapeHelper)
import           Queries

getAllBottles :: Connection -> IO [Bottle]
getAllBottles conn =
  runBeamSqlite conn $ runSelectReturningList $ select $ all_ (bottles wineDB)

getBottleByID :: Connection -> Int32 -> IO (Maybe Bottle)
getBottleByID conn bid =
  runBeamSqlite conn $
  runSelectReturningOne $
  select $
  filter_ (\b -> _bottleId b ==. val_ bid) $
  all_ (bottles wineDB)

getBottleByPoletID :: Connection -> Int32 -> IO (Maybe Bottle)
getBottleByPoletID conn pid =
  runBeamSqlite conn $
  runSelectReturningOne $
  select $
  filter_ (\b -> _bottlePoletId b ==. val_ (Just pid)) $
  all_ (bottles wineDB)

deleteBottleByID :: Connection -> Int32 -> IO ()
deleteBottleByID conn bid =
  runBeamSqlite conn $
  runDelete $
  delete (bottles wineDB)
  (\b -> _bottleId b ==. val_ bid)

insertBottle :: Connection -> Bottle -> IO ()
insertBottle conn bottle =
  runBeamSqlite conn $
  runInsert $
  insertOnConflict (bottles wineDB)
  (insertExpressions [ Bottle
                     { _bottleId          = if _bottleId bottle == -1 then default_ else val_ (_bottleId bottle)
                     , _bottlePoletId     = val_ $ _bottlePoletId bottle
                     , _bottleName        = val_ $ _bottleName bottle
                     , _bottleProducer    = val_ $ _bottleProducer bottle
                     , _bottleWineType    = val_ $ _bottleWineType bottle
                     , _bottleCountry     = val_ $ _bottleCountry bottle
                     , _bottleDistrict    = val_ $ _bottleDistrict bottle
                     , _bottleSubDistrict = val_ $ _bottleSubDistrict bottle
                     , _bottleVineyard    = val_ $ _bottleVineyard bottle
                     , _bottleVintage     = val_ $ _bottleVintage bottle
                     , _bottleCost        = val_ $ _bottleCost bottle
                     }
                   ])
    (conflictingFields primaryKey)
    (onConflictUpdateSet (\fields _ -> fields <-. val_ bottle))

insertGrapeProportions :: Connection -> [GrapeProportion] -> IO ()
insertGrapeProportions conn gproportions =
  runBeamSqlite conn $
  runInsert $
  insert (grape_proportions wineDB)
  (insertExpressions (map grapeHelper gproportions))

getGrapeProportionsByBottleID :: Connection -> Bottle -> IO [(Bottle, GrapeProportion)]
getGrapeProportionsByBottleID conn bot =
  runBeamSqlite conn $
  runSelectReturningList $
  select $
  filter_ (\(b, _) -> _bottleId b ==. val_ (_bottleId bot))
  allBottlesWithBlends

updateBottle :: Connection -> Bottle -> IO ()
updateBottle conn bottle =
  runBeamSqlite conn $
  runUpdate $
  save (bottles wineDB) bottle

updateGrapeProportion :: Connection -> GrapeProportion -> IO ()
updateGrapeProportion conn gproportion =
  runBeamSqlite conn $
  runUpdate $
  save (grape_proportions wineDB) gproportion