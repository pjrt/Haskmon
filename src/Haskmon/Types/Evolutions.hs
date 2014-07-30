module Haskmon.Types.Evolutions(
    module Haskmon.Types.Evolutions,
    I.Evolution, I.EvolutionMethod, I.LevelUpDetail
) where

import qualified Haskmon.Types.Internals as I

evolutionName :: I.Evolution -> String
evolutionName = I.evolutionName

evolutionMethod :: I.Evolution -> I.EvolutionMethod
evolutionMethod = I.evolutionMethod

evolutionPokemon :: I.Evolution -> IO I.Pokemon
evolutionPokemon = I.evolutionPokemon
