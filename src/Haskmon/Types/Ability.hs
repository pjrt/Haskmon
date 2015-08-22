module Haskmon.Types.Ability(
    module Haskmon.Types.Ability,
    I.Ability, I.MetaAbility
) where

import Haskmon.Types.Internals(MetaData, MetaAbility, Ability)
import qualified Haskmon.Types.Internals as I

mAbilityName :: MetaAbility -> String
mAbilityName = I.mAbilityName

getAbility :: MetaAbility -> IO Ability
getAbility = I.getAbility

abilityName :: Ability -> String
abilityName = I.abilityName

abilityDescription :: Ability -> String
abilityDescription = I.abilityDescription

abilityMetadata :: Ability -> MetaData
abilityMetadata = I.abilityMetadata
