module Queries where
import           Database
import           Database.Beam
import           Database.Beam.Sqlite

allBottlesWithBlends :: Q Sqlite WineDB s (BottleT (QExpr Sqlite s), GrapeProportionT (QExpr Sqlite s))
allBottlesWithBlends = do
  b <- all_ (bottles wineDB)
  gps <- oneToMany_ (grape_proportions wineDB) _grapeproportionBottle b
  pure (b, gps)
