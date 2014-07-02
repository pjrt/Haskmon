module Haskmon.Client(
              getPokedex,
              getPokemonById,
              getPokemonByName,
              getAbilityById,
              getMoveById
) where

import Haskmon.Types
import Haskmon.Resource(getResource)
import Data.Word
import Data.Aeson(FromJSON)
import Data.List(find)
import Data.Char(toLower)
import Control.Applicative((<$>))
import qualified Data.Traversable as T

type ID = Word

-- | Get a pokedex. Warning: returns a large list of pokemon resources
getPokedex :: IO Pokedex
getPokedex = getResourceById "pokedex" 1 -- At the moment, there is only one pokedex. ID 1

-- | Get a pokemon by ID
getPokemonById :: ID -> IO Pokemon
getPokemonById = getResourceById "pokemon"

-- | Lookup a pokemon by name
--   WARNING: Calls the pokedex api which returns a big list. It then does a lookup by name on said list.
getPokemonByName :: String -- ^ Name of the pokemon
                    -> IO (Maybe Pokemon) -- ^ If no pokemon is found, return IO Nothing
getPokemonByName name = do
              metaPkms <- pokedexPokemons <$> getPokedex
              let lName = toLower <$> name -- Lowercase it. All names in the pokedex meta are lower
                  maybePk = find ( (==) lName . mpName) metaPkms
              T.sequence $ fmap getPokemon maybePk

-- | Get an ability by ID
getAbilityById :: ID -> IO Ability
getAbilityById = getResourceById "ability"

-- | Get a move by ID
getMoveById :: ID -> IO Move
getMoveById = getResourceById "move"



-- | Utility wrapper for `ById` functions
getResourceById :: FromJSON a => String -> ID -> IO a
getResourceById res dbId = getResource $ "api/v1/" ++ res ++ "/" ++ show dbId ++ "/"

