Haskmon
=======

A Haskell wrapper for Pokeapi.co WIP
This library does not contain all the fields for all the pokémon in the
database. PRs are welcome.

Example usage:

```haskell
>>> import Haskmon
>>> getPokemonByName "eevee" :: IO Pokemon
Pokemon {pokemonName = "Eevee", pokemonNationalId = 133, ... }
>>> getMoveById 1 :: IO Move
Move {moveName = "Pound", movePower = 40, movePp = 35, moveAccuracy = 100, moveMetadata = MetaData {resourceUri = "/api/v1/move/1/", created = 2013-11-03 15:06:09.478009 UTC, modified = 2013-12-24 15:24:29.625596 UTC}}
```
