-- Resource getter. Used by everything else
module Haskmon.Resource(getResource) where

import Data.Word
import Data.Aeson
import Data.Either
import Network.Http.Client
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Control.Applicative((<$>))

-- | Host of the API
host = "http://pokeapi.co/"

type Uri = String
getResource :: FromJSON a => Uri -> IO a
getResource uri = decodeAndErr <$> get (B8.pack uri') concatHandler
        where uri' = host ++ uri
              decodeAndErr :: FromJSON a => B.ByteString -> a
              decodeAndErr = either decodeErr id . eitherDecodeStrict'
              decodeErr err = error $ "Error parsing resource " ++ uri' ++ ": " ++ err
