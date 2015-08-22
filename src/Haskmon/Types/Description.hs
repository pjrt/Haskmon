module Haskmon.Types.Description(
    module Haskmon.Types.Description,
    I.Description, I.MetaDescription
) where

import Haskmon.Types.Internals(MetaData, MetaPokemon, MetaGame, MetaDescription, Description)
import qualified Haskmon.Types.Internals as I

mDescriptionName :: MetaDescription -> String
mDescriptionName = I.mDescriptionName

getDescriptions :: MetaDescription -> IO Description
getDescriptions = I.getDescriptions


descriptionName :: Description -> String
descriptionName = I.descriptionName

descriptionText :: Description -> String
descriptionText = I.descriptionText

descriptionGames :: Description -> [MetaGame]
descriptionGames = I.descriptionGames

descriptionPokemon :: Description -> MetaPokemon
descriptionPokemon = I.descriptionPokemon

descriptionMetadata :: Description -> MetaData
descriptionMetadata = I.descriptionMetadata

