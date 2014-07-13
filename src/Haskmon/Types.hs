-- vim:fdm=marker
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

-- | All of the types returned by the API. From Pokemons to Abilities
-- Just like the API, the data fields have a nested pointer (IO a) to the reasource
-- it contains. For example, a Pokedex does not contain Pokemons, it contains
-- MetaPokemons (a name and a IO Pokemon function to actually get the pokemon).

module Haskmon.Types where

import Data.Word
import Data.Vector(toList)
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Format
import Data.Time.Clock
import Data.List(groupBy)
import Data.Function(on)

import System.Locale

import Control.Applicative
import Control.Monad(mzero)

import Haskmon.Resource(getResource)

-- {{{ MetaData
-- | Metadata that all resources (except Pokedex) have in common
data MetaData = MetaData {
              resourceUri :: String,
              created :: UTCTime,
              modified :: UTCTime
              }

getMetadata :: Object -> Parser MetaData
getMetadata v = MetaData <$>
                    v .: "resource_uri" <*>
                    convert (v .: "created") <*>
                    convert (v .: "modified")
              where convert :: Parser String -> Parser UTCTime
                    convert ps = readTime defaultTimeLocale formatStr <$> ps
                    formatStr = "%FT%H:%M:%Q"
-- }}}
-- Pokedex {{{
data Pokedex = Pokedex {
               pokedexName :: String,
               pokedexPokemons :: [MetaPokemon]
               }

data MetaPokemon = MetaPokemon { mpName :: String, getPokemon :: IO Pokemon }

instance Show Pokedex where
  show p = "<Pokedex - " ++ pokedexName p ++ ">"

instance FromJSON MetaPokemon where
  parseJSON (Object o) = MetaPokemon <$> o .: "name" <*> (getResource <$> o .: "resource_uri")

instance FromJSON Pokedex where
  parseJSON (Object o) = Pokedex <$> o .: "name" <*> o .: "pokemon"
  parseJSON _ = mzero

-- }}}
-- Pokemon {{{
data Pokemon = Pokemon {
             pokemonName :: String,
             nationalId :: Word,
             abilities :: [MetaAbility],
             moves :: [MetaMove],
             types :: [MetaType],
             eggCycle :: Word,
             eggGroups :: [MetaEgg],
             catchRate :: Word,
             hp :: Word,
             attack :: Word,
             defense :: Word,
             spAtk :: Word,
             spDef :: Word,
             speed :: Word,
             sprites :: [MetaSprite],
             descriptions :: MetaDescriptionList,
             metadata :: MetaData
             }

instance Show Pokemon where
  show p = "<Pokemon - " ++ pokemonName p ++ ">"

instance FromJSON Pokemon where
        parseJSON (Object v) = Pokemon <$>
                                v .: "name" <*>
                                v .: "national_id" <*>
                                v .: "abilities" <*>
                                v .: "moves" <*>
                                v .: "types" <*>
                                v .: "egg_cycles" <*>
                                v .: "egg_groups" <*>
                                v .: "catch_rate" <*>
                                v .: "hp" <*>
                                v .: "attack" <*>
                                v .: "defense" <*>
                                v .: "sp_atk" <*>
                                v .: "sp_def" <*>
                                v .: "speed" <*>
                                v .: "sprites" <*>
                                v .: "descriptions" <*>
                                getMetadata v
        parseJSON _ = mzero


--- }}}
-- Ability {{{
data MetaAbility = MetaAbility { mAbilityName :: String, getAbility :: IO Ability}
data Ability = Ability { abilityName :: String,
                         abilityDescription :: String,
                         abilityMetadata :: MetaData
                       }

instance Show Ability where
  show a = "<Ability - " ++ abilityName a ++ ">"

instance FromJSON Ability where
        parseJSON (Object v) = Ability <$>
                                v .: "name" <*>
                                v .: "description" <*>
                                getMetadata v
        parseJSON _ = mzero

instance FromJSON MetaAbility where
        parseJSON (Object o) = MetaAbility <$> o .: "name" <*>
                                           (getResource <$> o .: "resource_uri")
        parseJSON _ = mzero

-- }}}
-- Types {{{
data MetaType = MetaType { mTypeName :: String, getType :: IO Type }
data Type = Type {
          typeName :: String,
          ineffective :: [Type],
          noEffect :: [Type],
          resistance :: [Type],
          superEffective :: [Type],
          weakness :: [Type],
          typeMetadata :: MetaData
          }

instance Show Type where
  show t = "<Type - " ++ typeName t ++ ">"

instance FromJSON Type where
        parseJSON (Object o) = Type <$>
                                o .: "name" <*>
                                o .: "ineffective" <*>
                                o .: "no_effect" <*>
                                o .: "resistance" <*>
                                o .: "super_effective" <*>
                                o .: "weakness" <*>
                                getMetadata o
        parseJSON _ = mzero

instance FromJSON MetaType where
        parseJSON (Object o) = MetaType <$> o .: "name" <*>
                                            (getResource <$> o .: "resource_uri")
        parseJSON _ = mzero

-- }}}
-- Moves {{{
data MetaMove = MetaMove { mMoveName :: String,
                           mMoveLearnType :: String,
                           mLevel :: Maybe Word,
                           getMove :: IO Move}
data Move = Move {
          moveName :: String,
          power :: Int,
          pp :: Int,
          accuracy :: Int,
          moveMetadata :: MetaData
          }

instance Show Move where
  show m = "<Move - " ++ moveName m ++ ">"

instance FromJSON Move where
        parseJSON (Object o) = Move <$>
                                o .: "name" <*>
                                o .: "power" <*>
                                o .: "pp" <*>
                                o .: "accuracy" <*>
                                getMetadata o
        parseJSON _ = mzero

instance FromJSON MetaMove where
        parseJSON (Object o) = MetaMove <$> o .: "name" <*>
                                            o .: "learn_type" <*>
                                            o .:? "level" <*>
                                            (getResource <$> o .: "resource_uri")
        parseJSON _ = mzero

-- }}}
-- EGGS!!! {{{
data MetaEgg = MetaEgg { mEggGroupName :: String,
                         getEggGroup :: IO EggGroup
                       }
data EggGroup = EggGroup { eggGroupName :: String,
                           eggGroupPokemons :: [MetaPokemon],
                           eggGroupMetadata :: MetaData
                         }

instance Show EggGroup where
  show e = "<EggGroup - " ++ eggGroupName e ++ ">"

instance FromJSON EggGroup where
  parseJSON (Object o) = EggGroup <$>
                         o .: "name" <*>
                         o .: "pokemon" <*>
                         getMetadata o

  parseJSON _ = mzero

instance FromJSON MetaEgg where
  parseJSON (Object o) = MetaEgg <$>
                         o .: "name" <*>
                         (getResource <$> o .: "resource_uri")
  parseJSON _ = mzero
--- }}}
-- Description {{{
-- | MetaDescription come as a list of ordered sets. These are created by joining
-- together the elements that have the same "name", meaning that they are joined by the descriptions' generation.
-- At the moment, the meta data in the pokemon response does not contain the for the description, only the generation.
newtype MetaDescriptionList = MetaDescriptionList { metaDescriptionSets :: [MetaDescriptionSet] } deriving Show
data MetaDescriptionSet = MetaDescriptionSet {
                            mDescriptionName :: String,
                            getDescriptions :: [IO Description]
                          }
data Description = Description {
                    descriptionName :: String,
                    games :: [MetaGame],
                    descriptionPokemon :: MetaPokemon,
                    descriptionMetadata :: MetaData
                   }

instance Show Description where
  show d = "<Description - " ++ descriptionName d ++ ">"

instance Show MetaDescriptionSet where
  show set = "<DescriptionSet - " ++ mDescriptionName set ++ ">"

instance FromJSON Description where
  parseJSON (Object o) = Description <$>
                            o .: "name" <*>
                            o .: "games" <*>
                            o .: "pokemon" <*>
                            getMetadata o
  parseJSON _ = mzero

instance FromJSON MetaDescriptionList where
  parseJSON (Array a) = MetaDescriptionList <$>
                            map groupSets . groupBy ((==) `on` mDescriptionName) <$> (mapM go $ toList a)
                  where go (Object o) = MetaDescriptionSet <$> o .: "name" <*> ((:[]) . getResource <$> o .: "resource_uri")
                        go _ = mzero
                        groupSets :: [MetaDescriptionSet] -> MetaDescriptionSet
                        groupSets likeSets = foldr1 go' likeSets
                                where go' (MetaDescriptionSet _ xs) acc@(MetaDescriptionSet _ ys) = acc { getDescriptions = xs ++ ys}

  parseJSON _ = mzero

-- }}}
--  Game {{{
data MetaGame = MetaGame { mGameName :: String, getGame :: IO Game }
data Game = Game {
             gameName :: String,
             gameGeneration :: Word,
             releaseYear :: Word,
             gameMetadata :: MetaData
            }

instance Show Game where
  show g = "<Game - " ++ gameName g ++ ">"

instance FromJSON MetaGame where
  parseJSON (Object o) = MetaGame <$>
                           o .: "name" <*> (getResource <$> o .: "resource_uri")
  parseJSON _ = mzero

instance FromJSON Game where
  parseJSON (Object o) = Game <$>
                          o .: "name"<*>
                          o .: "generation" <*>
                          o .: "release_year" <*>
                          getMetadata o
  parseJSON _ = mzero
-- }}}
-- Sprite {{{
data MetaSprite = MetaSprite { mSpriteName :: String, getSprite :: IO Sprite }
data Sprite = Sprite {
                spriteName :: String,
                spritePokemon :: MetaPokemon,
                spriteImage :: String -- Just a plain URI
              }

instance Show Sprite where
  show s = "<Sprite - " ++ spriteName s ++ ">"

instance FromJSON MetaSprite where
  parseJSON (Object o) = MetaSprite <$>
                            o .: "name" <*>
                            (getResource <$> o .: "resource_uri")
  parseJSON _ = mzero

instance FromJSON Sprite where
  parseJSON (Object o) = Sprite <$>
                            o .: "name" <*>
                            o .: "pokemon" <*>
                            o .: "image"
  parseJSON _ = mzero
-- }}}
