module PoletAPI where

import           Network.HTTP.Simple

getAllFields :: String -> IO ()
getAllFields s = do
  response <- httpLbs $ parseRequest_ $ "http://www.vinmonopolet.no/api/products/" ++ s ++ "?fields=FULL"
  print $ getResponseBody response

getGrapeBlend :: String -> IO ()
getGrapeBlend s = do
  response <- httpLbs $ parseRequest_ $ "http://www.vinmonopolet.no/api/products/" ++ s ++ "?fields=raastoff"
  print $ getResponseBody response
