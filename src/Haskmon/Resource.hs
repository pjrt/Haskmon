-- Resource getter. Used by everything else
module Haskmon.Resource(getResource) where

import Data.Aeson
import Network.Http.Client
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

-- | Host of the API
host :: String
host = "http://pokeapi.co/"

-- | Uri of the resource
type Uri = String

-- | GET the resource from the URI and parse it into one of the "Haskmon.Types".
-- Fails if the parsing fails (ie: the library needs to be updated).
getResource :: FromJSON a => Uri -> IO a
getResource uri = decodeAndErr <$> get (B8.pack uri') concatHandler
        where uri' = host ++ uri
              decodeAndErr :: FromJSON a => B.ByteString -> a
              decodeAndErr = either decodeErr id . eitherDecodeStrict'
              decodeErr err = error $ "Error parsing resource " ++ uri' ++ ": " ++ err
