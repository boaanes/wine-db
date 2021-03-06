import           Database.SQLite.Simple
import           Routes
import           Web.Scotty

initializeDB :: IO ()
initializeDB = do
  conn <- open "wine.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS bottles (id INTEGER PRIMARY KEY, polet_id INTEGER, name TEXT, producer TEXT, wine_type TEXT, country TEXT, district TEXT, sub_district TEXT, vineyard TEXT, vintage INTEGER, cost REAL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS grape_proportions (id INTEGER PRIMARY KEY, name TEXT, percentage REAL, bottle__id INTEGER)"
  close conn

main :: IO ()
main = do
  initializeDB
  conn <- open "wine.db"
  scotty 3000 $ routes conn
