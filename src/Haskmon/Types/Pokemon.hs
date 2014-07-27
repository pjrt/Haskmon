module Haskmon.Types.Pokemon(
      module Haskmon.Types.Pokemon,
      I.Pokemon, I.Pokedex, I.MetaPokemon
) where

import Data.Word
import Haskmon.Types.Internals(MetaData,
                               MetaPokemon,
                               MetaAbility,
                               MetaMove,
                               MetaType,
                               MetaSprite,
                               MetaEggGroup,
                               MetaDescription,
                               Pokedex,
                               Pokemon)
import qualified Haskmon.Types.Internals as I

-- Pokedex
pokedexName :: Pokedex -> String
pokedexName = I.pokedexName

pokedexPokemons :: Pokedex -> [MetaPokemon]
pokedexPokemons = I.pokedexPokemons

-- MetaPokemon
mPokemonName :: MetaPokemon -> String
mPokemonName = I.mPokemonName

getPokemon :: MetaPokemon -> IO Pokemon
getPokemon = I.getPokemon

-- Pokemon
pokemonName :: Pokemon -> String
pokemonName = I.pokemonName

pokemonNationalId :: Pokemon -> Word
pokemonNationalId = I.pokemonNationalId

pokemonAbilities :: Pokemon -> [MetaAbility]
pokemonAbilities = I.pokemonAbilities

pokemonMoves :: Pokemon -> [MetaMove]
pokemonMoves = I.pokemonMoves

pokemonTypes :: Pokemon -> [MetaType]
pokemonTypes = I.pokemonTypes

pokemonEggCycle :: Pokemon -> Word
pokemonEggCycle = I.pokemonEggCycle

pokemonEggGroups :: Pokemon -> [MetaEggGroup]
pokemonEggGroups = I.pokemonEggGroups

pokemonCatchRate :: Pokemon -> Word
pokemonCatchRate = I.pokemonCatchRate

pokemonHp :: Pokemon -> Word
pokemonHp = I.pokemonHp

pokemonAttack :: Pokemon -> Word
pokemonAttack = I.pokemonAttack

pokemonDefense :: Pokemon -> Word
pokemonDefense = I.pokemonDefense

pokemonSpAtk :: Pokemon -> Word
pokemonSpAtk = I.pokemonSpAtk

pokemonSpDef :: Pokemon -> Word
pokemonSpDef = I.pokemonSpDef

pokemonSpeed :: Pokemon -> Word
pokemonSpeed = I.pokemonSpeed

pokemonSprites :: Pokemon -> [MetaSprite]
pokemonSprites = I.pokemonSprites

pokemonDescriptions :: Pokemon -> [MetaDescription]
pokemonDescriptions = I.pokemonDescriptions

pokemonMetadata :: Pokemon -> MetaData
pokemonMetadata = I.pokemonMetadata
