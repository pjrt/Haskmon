module Haskmon.Types.MetaData(
   module Haskmon.Types.MetaData,
   I.MetaData
) where

import Data.Time.Clock
import Haskmon.Types.Internals(MetaData)
import qualified Haskmon.Types.Internals as I

resourceUri :: MetaData -> String
resourceUri = I.resourceUri

created :: MetaData -> UTCTime
created = I.created

modified :: MetaData -> UTCTime
modified = I.modified
