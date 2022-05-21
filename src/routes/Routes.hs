module Routes where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Aeson                (decode)
import           Data.Char
import           Data.Int
import qualified Data.Text.Internal.Lazy   as T
import qualified Data.Text.Lazy            as T
import           Database
import           Database.SQLite.Simple
import           Json                      (FullResponse (FullResponse))
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status
import           Polet
import           QueriesIO
import           Web.Scotty

findQueryParam :: T.Text -> [Param] -> Maybe T.Text
findQueryParam key qparams =
  lookup key qparams <|> Nothing

routes :: Connection -> ScottyM ()
routes conn = do
  get "/" $ do
    bs <- liftIO $ getAllBottles conn
    json bs

  get "/:id" $ do
    bid <- param "id" :: ActionM Int
    b <- liftIO $ getBottleByID conn (fromIntegral bid)
    case b of
      Just bot -> json bot
      Nothing  -> status notFound404 >> raw "Bottle not found"

  get "/poletid/:id" $ do
    bid <- param "id" :: ActionM Int
    qparams <- params
    let blend = case findQueryParam "blend" qparams of
          Just b  -> map toLower (T.unpack b) == "true"
          Nothing -> False

    b <- liftIO $ getBottleByPoletID conn (fromIntegral bid)
    case b of
      Just bot -> do
        if blend
          then do
            gp <- liftIO $ getGrapeProportionsByBottleID conn bot
            json $ FullResponse bot (map snd gp)
          else json bot
      Nothing  -> status notFound404 >> raw "Bottle not found"

  post "/" $ do
    bottle <- jsonData :: ActionM Bottle
    liftIO $ insertBottle conn bottle
    json bottle

  post "/:id" $ do
    bid <- param "id" :: ActionM String
    poletResponse <- liftIO $ httpLbs $ parseRequest_ $ "http://www.vinmonopolet.no/api/products/" ++ bid ++ "?fields=FULL"
    case decode (getResponseBody poletResponse) :: Maybe PoletResponse of
      Nothing          -> status notFound404 >> raw "Bottle does not exist in Vinmonopolet"
      Just poletBottle -> do
        let bid' = read bid :: Int32

        let bottle = fromPoletResponseToBottle poletBottle
        liftIO $ insertBottle conn bottle

        Just bottle' <- liftIO $ getBottleByPoletID conn bid'
        let grapeProportions = fromPoletResponseToGrapeProportions poletBottle bottle'
        liftIO $ insertGrapeProportions conn grapeProportions

        grapeProportions' <- liftIO $ getGrapeProportionsByBottleID conn bottle'
        json $ FullResponse bottle' (map snd grapeProportions')

  delete "/:id" $ do
    bid <- param "id" :: ActionM Int
    liftIO $ deleteBottleByID conn (fromIntegral bid)
    json ()

  put "/" $ do
    bottle <- jsonData :: ActionM Bottle
    liftIO $ updateBottle conn bottle
    json bottle
