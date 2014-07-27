module Haskmon.Types.EggGroup(
    module Haskmon.Types.EggGroup,
    I.EggGroup, I.MetaEggGroup
) where

import Data.Word
import Haskmon.Types.Internals(MetaData, MetaEggGroup, EggGroup, MetaPokemon)
import qualified Haskmon.Types.Internals as I


mEggGroupName :: MetaEggGroup -> String
mEggGroupName = I.mEggGroupName

getEggGroup :: MetaEggGroup -> IO EggGroup
getEggGroup = I.getEggGroup


eggGroupName :: EggGroup -> String
eggGroupName = I.eggGroupName

eggGroupPokemons :: EggGroup -> [MetaPokemon]
eggGroupPokemons = I.eggGroupPokemons

eggGroupMetadata :: EggGroup -> MetaData
eggGroupMetadata = I.eggGroupMetadata
