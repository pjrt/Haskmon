-- vim:fdm=marker
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

-- | All of the types returned by the API. From Pokemons to Abilities
-- Just like the API, the data fields have a nested pointer (IO a) to the resource
-- it contains. For example, a Pokedex does not contain Pokemons, it contains
-- MetaPokemons (a name and a IO Pokemon function to actually get the pokemon).

module Haskmon.Types(
                    -- {{{ Exports
                    -- All types are exported with their functions, but not constructors
                  MetaData(
                      resourceUri, created, modified
                  ),

                  Pokemon(
                      pokemonName , pokemonNationalId , pokemonAbilities , pokemonMoves , pokemonTypes , pokemonEggCycle
                    , pokemonEggGroups , pokemonCatchRate , pokemonHp , pokemonAttack , pokemonDefense , pokemonSpAtk
                    , pokemonSpDef , pokemonSpeed , pokemonSprites , pokemonDescriptions , pokemonMetadata
                  )

                  , MetaPokemon(
                    mPokemonName , getPokemon
                  )

                  , Pokedex(
                    pokedexName , pokedexPokemons
                  )

                  , MetaAbility(
                    mAbilityName , getAbility
                  )

                  , Ability(
                    abilityName , abilityDescription , abilityMetadata
                  )

                  , MetaType(
                    mTypeName , getType
                  )

                  , Type(
                    typeName , typeIneffective , typeNoEffect , typeResistance , typeSuperEffective
                  , typeWeakness , typeMetadata
                  )

                  , MetaMove(
                    mMoveName , mMoveLearnType , getMove
                  )

                  , Move(
                    moveName , movePower , movePp , moveAccuracy , moveMetadata
                  )

                  , MetaEggGroup(
                    mEggGroupName , getEggGroup
                  )

                  , EggGroup(
                    eggGroupName , eggGroupPokemons , eggGroupMetadata
                  )

                  , MetaDescription(
                    mDescriptionName , getDescriptions
                  )

                  , Description(
                    descriptionName , descriptionText , descriptionGames , descriptionPokemon , descriptionMetadata
                  )

                  , MetaGame(
                    mGameName , getGame
                  )

                  , Game(
                    gameName , gameGeneration, gameReleaseYear, gameMetadata
                  )

                  , MetaSprite(
                    mSpriteName, getSprite
                  )

                  , Sprite(
                    spriteName, spritePokemon, spriteImage
                  )
                  -- }}}
) where

import Data.Word
import Data.Vector(toList)
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Format
import Data.Time.Clock
import Data.List(find)

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
                } deriving Show

getMetadata :: Object -> Parser MetaData
getMetadata v = MetaData <$>
                    v .: "resource_uri" <*>
                    convert (v .: "created") <*>
                    convert (v .: "modified")
              where convert :: Parser String -> Parser UTCTime
                    convert ps = readTime defaultTimeLocale formatStr <$> ps
                    formatStr = "%FT%R:%S%Q"

-- Alias for withObject
withO o f = withObject "object" f o

-- }}}
-- Pokedex {{{
data Pokedex = Pokedex {
               pokedexName :: String,
               pokedexPokemons :: [MetaPokemon]
               }

instance Show Pokedex where
  show p = "<Pokedex - " ++ pokedexName p ++ ">"

instance FromJSON MetaPokemon where
  parseJSON (Object o) = MetaPokemon <$> o .: "name" <*> (getResource <$> o .: "resource_uri")

instance FromJSON Pokedex where
  parseJSON (Object o) = Pokedex <$> o .: "name" <*> o .: "pokemon"
  parseJSON _ = mzero

-- }}}
-- Pokemon {{{
data MetaPokemon = MetaPokemon { mPokemonName :: String, getPokemon :: IO Pokemon }
instance Show MetaPokemon where
  show p = "<MetaPokemon - " ++ mPokemonName p ++ ">"

data Pokemon = Pokemon {
                  pokemonName :: String,
                  pokemonNationalId :: Word,
                  pokemonAbilities :: [MetaAbility],
                  pokemonMoves :: [MetaMove],
                  pokemonTypes :: [MetaType],
                  pokemonEggCycle :: Word,
                  pokemonEggGroups :: [MetaEggGroup],
                  pokemonCatchRate :: Word,
                  pokemonHp :: Word,
                  pokemonAttack :: Word,
                  pokemonDefense :: Word,
                  pokemonSpAtk :: Word,
                  pokemonSpDef :: Word,
                  pokemonSpeed :: Word,
                  pokemonSprites :: [MetaSprite],
                  pokemonDescriptions :: [MetaDescription],
                  pokemonMetadata :: MetaData
             } deriving Show

instance FromJSON Pokemon where
        parseJSON o = withO o go
                       where go v = Pokemon <$>
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


--- }}}
-- Evolution {{{
{- At the moment, evolutions don't seem to be very consistent or complete (in the Rest API).
 - Ignoring for now
data Evolution = Evolution {
                    evolutionName :: String,
                    evolutionMethod :: EvolutionMethod
                    evolutionPokemon :: IO Pokemon
                 }
data EvolutionMethod = LevelUp { evolutionLevel :: Word }
                     | Friendship
-}
-- }}}
-- Ability {{{
data MetaAbility = MetaAbility { mAbilityName :: String, getAbility :: IO Ability}
instance Show MetaAbility where
  show p = "<MetaAbility - " ++ mAbilityName p ++ ">"

data Ability = Ability { abilityName :: String,
                         abilityDescription :: String,
                         abilityMetadata :: MetaData
                       } deriving Show

instance FromJSON Ability where
        parseJSON o = withO o $ \v -> Ability <$>
                                v .: "name" <*>
                                v .: "description" <*>
                                getMetadata v

instance FromJSON MetaAbility where
        parseJSON v = withO v $ \o -> MetaAbility <$> o .: "name" <*>
                                           (getResource <$> o .: "resource_uri")

-- }}}
-- Types {{{
data MetaType = MetaType { mTypeName :: String, getType :: IO Type }
instance Show MetaType where
  show p = "<MetaType - " ++ mTypeName p ++ ">"

data Type = Type {
            typeName :: String,
            typeIneffective :: [Type],
            typeNoEffect :: [Type],
            typeResistance :: [Type],
            typeSuperEffective :: [Type],
            typeWeakness :: [Type],
            typeMetadata :: MetaData
          } deriving Show

instance FromJSON Type where
        parseJSON v = withO v $ \o -> Type <$>
                                o .: "name" <*>
                                o .: "ineffective" <*>
                                o .: "no_effect" <*>
                                o .: "resistance" <*>
                                o .: "super_effective" <*>
                                o .: "weakness" <*>
                                getMetadata o

instance FromJSON MetaType where
        parseJSON v = withO v $ \o -> MetaType <$> o .: "name" <*>
                                            (getResource <$> o .: "resource_uri")

-- }}}
-- Moves {{{
data MetaMoveLearnType = LevelUp Int
                       | Machine
                       | Tutor
                       | EggMove
                       | Other  deriving Show

data MetaMove = MetaMove { mMoveName :: String,
                           mMoveLearnType :: MetaMoveLearnType,
                           getMove :: IO Move}
instance Show MetaMove where
  show p = "<MetaMove - " ++ mMoveName p ++ ">"

data Move = Move {
              moveName :: String,
              movePower :: Word,
              movePp :: Word,
              moveAccuracy :: Word,
              moveMetadata :: MetaData
            } deriving Show

instance FromJSON Move where
        parseJSON v = withO v $ \o -> Move <$>
                                o .: "name" <*>
                                o .: "power" <*>
                                o .: "pp" <*>
                                o .: "accuracy" <*>
                                getMetadata o

instance FromJSON MetaMove where
        parseJSON v = withO v go
                       where go o = MetaMove <$> o .: "name" <*>
                                            mmLearnType <*>
                                            (getResource <$> o .: "resource_uri")
                                        where mmLearnType :: Parser MetaMoveLearnType
                                              mmLearnType = do
                                                          learnType <- o .: "learn_type" :: Parser String
                                                          case learnType of
                                                              "level up" -> LevelUp <$> (o .: "level")
                                                              "machine" -> return Machine
                                                              "tutor" -> return Tutor
                                                              "egg move" -> return EggMove
                                                              "other" -> return Other
                                                              err -> fail $  "expected level_up, machine, tutor, egg_move or other. Got " ++ err

-- }}}
-- EGGS!!! {{{
data MetaEggGroup = MetaEggGroup {
                         mEggGroupName :: String,
                         getEggGroup :: IO EggGroup
                       }
instance Show MetaEggGroup where
  show p = "<MetaEggGroup - " ++ mEggGroupName p ++ ">"

data EggGroup = EggGroup { eggGroupName :: String,
                           eggGroupPokemons :: [MetaPokemon],
                           eggGroupMetadata :: MetaData
                         } deriving Show

instance FromJSON EggGroup where
  parseJSON v = withO v $ \o -> EggGroup <$>
                         o .: "name" <*>
                         o .: "pokemon" <*>
                         getMetadata o


instance FromJSON MetaEggGroup where
  parseJSON v = withO v $ \o -> MetaEggGroup <$>
                         o .: "name" <*>
                         (getResource <$> o .: "resource_uri")
--- }}}
-- Description {{{
data MetaDescription = MetaDescription {
                            mDescriptionName :: String,
                            getDescriptions :: IO Description
                          }

data Description = Description {
                    descriptionName :: String,
                    descriptionText :: String,
                    descriptionGames :: [MetaGame],
                    descriptionPokemon :: MetaPokemon,
                    descriptionMetadata :: MetaData
                   } deriving Show

instance Show MetaDescription where
  show set = "<MetaDescription - " ++ mDescriptionName set ++ ">"

instance FromJSON Description where
  parseJSON v = withO v $ \o -> Description <$>
                            o .: "name" <*>
                            o .: "description" <*>
                            o .: "games" <*>
                            o .: "pokemon" <*>
                            getMetadata o

instance FromJSON MetaDescription where
  parseJSON v = withO v $ \o -> MetaDescription <$>
                            o .: "name" <*>
                            (getResource <$> o .: "resource_uri")


-- }}}
--  Game {{{
data MetaGame = MetaGame { mGameName :: String, getGame :: IO Game }
instance Show MetaGame where
  show set = "<MetaGame - " ++ mGameName set ++ ">"

data Game = Game {
             gameName :: String,
             gameGeneration :: Word,
             gameReleaseYear :: Word,
             gameMetadata :: MetaData
            } deriving Show

instance FromJSON MetaGame where
  parseJSON v = withO v $ \o -> MetaGame <$>
                           o .: "name" <*> (getResource <$> o .: "resource_uri")

instance FromJSON Game where
  parseJSON v = withO v $ \o -> Game <$>
                          o .: "name"<*>
                          o .: "generation" <*>
                          o .: "release_year" <*>
                          getMetadata o
-- }}}
-- Sprite {{{
data MetaSprite = MetaSprite { mSpriteName :: String, getSprite :: IO Sprite }
instance Show MetaSprite where
  show set = "<MetaSprite - " ++ mSpriteName set ++ ">"

data Sprite = Sprite {
                spriteName :: String,
                spritePokemon :: MetaPokemon,
                spriteImage :: String -- Just a plain URI
              } deriving Show

instance FromJSON MetaSprite where
  parseJSON v = withO v $ \o -> MetaSprite <$>
                            o .: "name" <*>
                            (getResource <$> o .: "resource_uri")

instance FromJSON Sprite where
  parseJSON v = withO v $ \o -> Sprite <$>
                            o .: "name" <*>
                            o .: "pokemon" <*>
                            o .: "image"
-- }}}
