module Haskmon.Client(
              getPokedexById,
              getNationalPokedex,
              getPokemonById,
              getPokemonByName,
              getAbilityById,
              getMoveById,
              getTypeById,
              getEggGroupById,
              getDescriptionById,
              getGameById,
              getSpriteById
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

-- | Utility wrapper for `ById` functions
getResourceById :: FromJSON a => String -> String -> IO a
getResourceById res dbId = getResource $ "api/v1/" ++ res ++ "/" ++ dbId ++ "/"

----------------------------------------------------------------------------

-- | Get a pokedex. Warning: returns a large list of pokemon resources
getPokedexById :: ID -> IO Pokedex
getPokedexById = getResourceById "pokedex" . show

getNationalPokedex :: IO Pokedex
getNationalPokedex = getPokedexById 1 -- At the moment, this is the only pokedex

-- | Get a pokemon by ID
getPokemonById :: ID -> IO Pokemon
getPokemonById = getResourceById "pokemon" . show

-- | Lookup a pokemon by name in the national pokedex
getPokemonByName :: String -- ^ Name of the pokemon
                    -> IO Pokemon
getPokemonByName = getResourceById "pokemon"

-- | Get an ability by ID
getAbilityById :: ID -> IO Ability
getAbilityById = getResourceById "ability" . show

-- | Get a move by ID
getMoveById :: ID -> IO Move
getMoveById = getResourceById "move" . show

-- | Get a type by ID
getTypeById :: ID -> IO Type
getTypeById = getResourceById "type" . show
--
-- | Get a egg group by ID
getEggGroupById :: ID -> IO EggGroup
getEggGroupById = getResourceById "egg" . show

-- | Get a description group by ID
getDescriptionById :: ID -> IO Description
getDescriptionById = getResourceById "description" . show

-- | Get a game by ID
getGameById :: ID -> IO Description
getGameById = getResourceById "game" . show

-- | Get Sprite by ID
getSpriteById :: ID -> IO Sprite
getSpriteById = getResourceById "sprite" . show


