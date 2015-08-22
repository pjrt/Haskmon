module Haskmon.Types.Type(
    module Haskmon.Types.Type,
    I.Type, I.MetaType
) where

import Haskmon.Types.Internals(MetaData, MetaType, Type)
import qualified Haskmon.Types.Internals as I

mTypeName :: MetaType -> String
mTypeName = I.mTypeName

getType :: MetaType -> IO Type
getType = I.getType

typeName :: Type -> String
typeName = I.typeName

typeIneffective :: Type -> [Type]
typeIneffective = I.typeIneffective

typeNoEffect :: Type -> [Type]
typeNoEffect = I.typeNoEffect

typeResistance :: Type -> [Type]
typeResistance = I.typeResistance

typeSuperEffective :: Type -> [Type]
typeSuperEffective = I.typeSuperEffective

typeWeakness :: Type -> [Type]
typeWeakness = I.typeWeakness

typeMetadata :: Type -> MetaData
typeMetadata = I.typeMetadata

