module Haskmon.Types.Game where

import Haskmon.Types.Internals(MetaData, MetaGame, Game)
import qualified Haskmon.Types.Internals as I

mGameName :: MetaGame -> String
mGameName = I.mGameName

getGame :: MetaGame -> IO Game
getGame = I.getGame

gameName :: Game -> String
gameName = I.gameName

gameGeneration :: Game -> Word
gameGeneration = I.gameGeneration

gameReleaseYear :: Game -> Word
gameReleaseYear = I.gameReleaseYear

gameMetadata :: Game -> MetaData
gameMetadata = I.gameMetadata
