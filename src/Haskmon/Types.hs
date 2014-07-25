module Haskmon.Types (
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
                    mpName , getPokemon
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
)

import qualified Haskmon.Types.Intenals as T

resourceUri :: MetaData -> String
resourceUri = T.resourceUri
created :: MetaData -> UTCTime
created = T.created
modified :: MetaData -> UTCTime
modified = T.modified
pokedexName :: Pokedex -> String
pokedexName = T.pokedexName
pokedexPokemons :: Pokedex -> [MetaPokemon]
pokedexPokemons = T.pokedexPokemons
mpName :: MetaPokemon -> String
mpName = T.mpName
getPokemon :: MetaPokemon -> IO Pokemon
getPokemon = T.getPokemon
pokemonName :: Pokemon -> String
pokemonName = T.pokemonName
pokemonNationalId :: Pokemon -> Word
pokemonNationalId = T.pokemonNationalId
pokemonAbilities :: Pokemon -> [MetaAbility]
pokemonAbilities = T.pokemonAbilities
pokemonMoves :: Pokemon -> [MetaMove]
pokemonMoves = T.pokemonMoves
pokemonTypes :: Pokemon -> [MetaType]
pokemonTypes = T.pokemonTypes
pokemonEggCycle :: Pokemon -> Word
pokemonEggCycle = T.pokemonEggCycle
pokemonEggGroups :: Pokemon -> [MetaEggGroup]
pokemonEggGroups = T.pokemonEggGroups
pokemonCatchRate :: Pokemon -> Word
pokemonCatchRate = T.pokemonCatchRate
pokemonHp :: Pokemon -> Word
pokemonHp = T.pokemonHp
pokemonAttack :: Pokemon -> Word
pokemonAttack = T.pokemonAttack
pokemonDefense :: Pokemon -> Word
pokemonDefense = T.pokemonDefense
pokemonSpAtk :: Pokemon -> Word
pokemonSpAtk = T.pokemonSpAtk
pokemonSpDef :: Pokemon -> Word
pokemonSpDef = T.pokemonSpDef
pokemonSpeed :: Pokemon -> Word
pokemonSpeed = T.pokemonSpeed
pokemonSprites :: Pokemon -> [MetaSprite]
pokemonSprites = T.pokemonSprites
pokemonDescriptions :: Pokemon -> [MetaDescription]
pokemonDescriptions = T.pokemonDescriptions
pokemonMetadata :: Pokemon -> MetaData
pokemonMetadata = T.pokemonMetadata
evolutionName :: Evolution -> String
evolutionName = T.evolutionName
evolutionMethod :: Evolution -> EvolutionMethod
evolutionMethod = T.evolutionMethod
evolutionPokemon :: Evolution -> IO Pokemon
evolutionPokemon = T.evolutionPokemon
evolutionLevel :: EvolutionMethod -> Word
evolutionLevel = T.evolutionLevel
mAbilityName :: MetaAbility -> String
mAbilityName = T.mAbilityName
getAbility :: MetaAbility -> IO Ability
getAbility = T.getAbility
abilityName :: Ability -> String
abilityName = T.abilityName
abilityDescription :: Ability -> String
abilityDescription = T.abilityDescription
abilityMetadata :: Ability -> MetaData
abilityMetadata = T.abilityMetadata
mTypeName :: MetaType -> String
mTypeName = T.mTypeName
getType :: MetaType -> IO Type
getType = T.getType
typeName :: Type -> String
typeName = T.typeName
typeIneffective :: Type -> [Type]
typeIneffective = T.typeIneffective
typeNoEffect :: Type -> [Type]
typeNoEffect = T.typeNoEffect
typeResistance :: Type -> [Type]
typeResistance = T.typeResistance
typeSuperEffective :: Type -> [Type]
typeSuperEffective = T.typeSuperEffective
typeWeakness :: Type -> [Type]
typeWeakness = T.typeWeakness
typeMetadata :: Type -> MetaData
typeMetadata = T.typeMetadata
mMoveName :: MetaMove -> String
mMoveName = T.mMoveName
mMoveLearnType :: MetaMove -> MetaMoveLearnType
mMoveLearnType = T.mMoveLearnType
getMove :: MetaMove -> IO Move
getMove = T.getMove
moveName :: Move -> String
moveName = T.moveName
movePower :: Move -> Int
movePower = T.movePower
movePp :: Move -> Int
movePp = T.movePp
moveAccuracy :: Move -> Int
moveAccuracy = T.moveAccuracy
moveMetadata :: Move -> MetaData
moveMetadata = T.moveMetadata
mEggGroupName :: MetaEggGroup -> String
mEggGroupName = T.mEggGroupName
getEggGroup :: MetaEggGroup -> IO EggGroup
getEggGroup = T.getEggGroup
eggGroupName :: EggGroup -> String
eggGroupName = T.eggGroupName
eggGroupPokemons :: EggGroup -> [MetaPokemon]
eggGroupPokemons = T.eggGroupPokemons
eggGroupMetadata :: EggGroup -> MetaData
eggGroupMetadata = T.eggGroupMetadata
mDescriptionName :: MetaDescription -> String
mDescriptionName = T.mDescriptionName
getDescriptions :: MetaDescription -> IO Description
getDescriptions = T.getDescriptions
descriptionName :: Description -> String
descriptionName = T.descriptionName
descriptionText :: Description -> String
descriptionText = T.descriptionText
descriptionGames :: Description -> [MetaGame]
descriptionGames = T.descriptionGames
descriptionPokemon :: Description -> MetaPokemon
descriptionPokemon = T.descriptionPokemon
descriptionMetadata :: Description -> MetaData
descriptionMetadata = T.descriptionMetadata
mGameName :: MetaGame -> String
mGameName = T.mGameName
getGame :: MetaGame -> IO Game
getGame = T.getGame
gameName :: Game -> String
gameName = T.gameName
gameGeneration :: Game -> Word
gameGeneration = T.gameGeneration
gameReleaseYear :: Game -> Word
gameReleaseYear = T.gameReleaseYear
gameMetadata :: Game -> MetaData
gameMetadata = T.gameMetadata
mSpriteName :: MetaSprite -> String
mSpriteName = T.mSpriteName
getSprite :: MetaSprite -> IO Sprite
getSprite = T.getSprite
spriteName :: Sprite -> String
spriteName = T.spriteName
spritePokemon :: Sprite -> MetaPokemon
spritePokemon = T.spritePokemon
spriteImage :: Sprite -> String
spriteImage = T.spriteImage

