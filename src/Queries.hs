module Queries where

import           Database.SQLite.Simple

createBottlesTableQuery :: Query
createBottlesTableQuery = "CREATE TABLE IF NOT EXISTS bottles (id INTEGER PRIMARY KEY, name TEXT, producer TEXT, wineType TEXT, country TEXT, region TEXT, subRegion TEXT, vineyard TEXT, vintage INTEGER, cost INTEGER)"

insertBottleQuery :: Query
insertBottleQuery = "INSERT INTO bottles (name, producer, wineType, country, region, subRegion, vineyard, vintage, cost) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"

getAllBottlesQuery :: Query
getAllBottlesQuery = "SELECT name, producer, wineType, country, region, subRegion, vineyard, vintage, cost FROM bottles"

getBottleByIDQuery :: Query
getBottleByIDQuery = "SELECT name, producer, wineType, country, region, subRegion, vineyard, vintage, cost FROM bottles WHERE id = :id"

deleteBottleByIDQuery :: Query
deleteBottleByIDQuery = "DELETE FROM bottles WHERE id = :id"

-- update bottle function using bottle record defined in Schema.hs
updateBottleByIDQuery :: Query
updateBottleByIDQuery = "UPDATE bottles SET name = :name, producer = :producer, wineType = :wineType, country = :country, region = :region, subRegion = :subRegion, vineyard = :vineyard, vintage = :vintage, cost = :cost WHERE id = :id"

