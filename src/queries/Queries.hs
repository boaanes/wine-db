module Queries where
import           Data.Int
import           Database
import           Database.Beam
import           Database.Beam.Sqlite

allBottles :: Q Sqlite WineDB s (BottleT (QExpr Sqlite s))
allBottles = all_ (bottles wineDB)

bottleById :: Int32 -> Q Sqlite WineDB s (BottleT (QExpr Sqlite s))
bottleById bid = filter_ (\b -> _bottleId b ==. val_ bid) allBottles

allBottlesWithBlends :: Q Sqlite WineDB s (BottleT (QExpr Sqlite s), GrapeProportionT (QExpr Sqlite s))
allBottlesWithBlends = do
  b <- all_ (bottles wineDB)
  gps <- oneToMany_ (grape_proportions wineDB) _grapeproportionBottle b
  pure (b, gps)

allGrapeProportions :: Q Sqlite WineDB s (GrapeProportionT (QExpr Sqlite s))
allGrapeProportions = all_ (grape_proportions wineDB)
