module Haskmon.Types.Move(
    module Haskmon.Types.Move,
    I.Move, I.MetaMove
) where

import Data.Word
import Haskmon.Types.Internals(MetaData, MetaMove, Move, MetaMoveLearnType)
import qualified Haskmon.Types.Internals as I

mMoveName :: MetaMove -> String
mMoveName = I.mMoveName

mMoveLearnType :: MetaMove -> MetaMoveLearnType
mMoveLearnType = I.mMoveLearnType

getMove :: MetaMove -> IO Move
getMove = I.getMove


moveName :: Move -> String
moveName = I.moveName

movePower :: Move -> Word
movePower = I.movePower

movePp :: Move -> Word
movePp = I.movePp

moveAccuracy :: Move -> Word
moveAccuracy = I.moveAccuracy

moveMetadata :: Move -> MetaData
moveMetadata = I.moveMetadata

