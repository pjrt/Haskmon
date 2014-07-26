-- | Examples:
--
-- >>> import Haskmon
-- >>> getPokemonByName "eevee" :: IO (Maybe Pokemon)
-- Just (Pokemon {pokemonName = "Eevee", pokemonNationalId = 133, ... })
-- >>> getMoveById 1 :: IO Move
-- Move {moveName = "Pound", movePower = 40, movePp = 35, moveAccuracy = 100, moveMetadata = MetaData {resourceUri = "/api/v1/move/1/", created = 2013-11-03 15:06:09.478009 UTC, modified = 2013-12-24 15:24:29.625596 UTC}}
--
-- For a full list of types (pokemons, moves, abilities, etc) see "Haskmon.Types".
-- For a full list of getter functions, see "Haskmon.Client".

module Haskmon (
    module Haskmon.Types,
    module Haskmon.Client
) where

import Haskmon.Types
import Haskmon.Client
