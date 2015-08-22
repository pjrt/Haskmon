module Haskmon.Types.Sprite(
    module Haskmon.Types.Sprite,
    I.Sprite, I.MetaSprite
) where

import Haskmon.Types.Internals(MetaPokemon, MetaSprite, Sprite)
import qualified Haskmon.Types.Internals as I

mSpriteName :: MetaSprite -> String
mSpriteName = I.mSpriteName

getSprite :: MetaSprite -> IO Sprite
getSprite = I.getSprite


spriteName :: Sprite -> String
spriteName = I.spriteName

spritePokemon :: Sprite -> MetaPokemon
spritePokemon = I.spritePokemon

spriteImage :: Sprite -> String
spriteImage = I.spriteImage
