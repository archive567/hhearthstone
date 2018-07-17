[hheartstone](https://github.com/tonyday567/hheartstone)
========================================================

Modelling Heartstone

Gabrielle on [game
simulation](http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html)

Game engine library (console not working in macos):
[heartshroud](https://github.com/thomaseding/hearthshroud)

Get decks from hearthpwm:
[blender](https://github.com/blender/Hearthstone) Card manager:
[HCM](https://github.com/nicuveo/HCM) Simulator in python:
[fireplace](https://github.com/jleclanche/fireplace)

Hearthstone AI
[discussion](https://www.reddit.com/r/hearthstone/comments/7l1ob0/i_wrote_a_masters_thesis_on_effective_hearthstone/)

game state
----------

test game render
----------------

turn: First player 1: Jaina Proudmoore mana: 10 deck left: 27 hand:
<img src='https://art.hearthstonejson.com/v1/render/latest/enUS/256x/EX1_584.png'>
<img src='https://art.hearthstonejson.com/v1/render/latest/enUS/256x/GIL_207.png'>
<img src='https://art.hearthstonejson.com/v1/render/latest/enUS/256x/OG_295.png'>
board: player 2: Malfurion Stormrage mana: 10 deck left: 26 hand:
<img src='https://art.hearthstonejson.com/v1/render/latest/enUS/256x/LOE_050.png'>
<img src='https://art.hearthstonejson.com/v1/render/latest/enUS/256x/FP1_027.png'>
<img src='https://art.hearthstonejson.com/v1/render/latest/enUS/256x/CS2_181.png'>
<img src='https://art.hearthstonejson.com/v1/render/latest/enUS/256x/EX1_573.png'>
<img src='https://art.hearthstonejson.com/v1/render/latest/enUS/256x/GAME_005.png'>
deck:

api
---

``` {.sourceCode .literate .haskell}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Hearth where

import Protolude hiding (First)
import System.Random.MWC
import Control.Monad.Primitive
import qualified Data.HashMap.Strict as Map
import GHC.Show

data Env = Env
  { gen :: Gen (PrimState IO)
  , allCards :: [Card]
  , collectibles :: [Card]
  , heros :: Map.HashMap CardClass Card
  , heroPowers :: Map.HashMap CardClass Card
  , theCoin :: Card
  }

type Id = Text

data Card = Card
  { cost :: Int
  , attack :: Int
  , health :: Int
  , id :: Id
  , name :: Text
  , mechanics :: [Mechanic]
  , cardType :: CardType
  , cardState :: [CardState]
  , cardClass :: CardClass
  , race :: Maybe Text
  , spellDamage :: Maybe Int
  , overload :: Maybe Int
  , durability :: Maybe Int
  , targetText :: Maybe Text
  , entourage :: Maybe [Id]
  , cardText :: Text
  } deriving (Show)

instance Eq Card where
  (==) a b = id a == id b

data TAction = TAction { tdesc :: Text, taction :: Target -> [Event]}

instance Show TAction where
  show (TAction t _) = GHC.Show.show t

data PAction = PAction { pdesc :: Text, paction :: Position -> TAction}

instance Show PAction where
  show (PAction t _) = GHC.Show.show t

data Mechanic =
  Charge |
  Taunt |
  Battlecry Text [TAction] |
  Deathrattle Text [TAction] |
  Power Text [PAction] |
  SpellPower [PAction] |
  Rush |
  DivineShield |
  Poisonous |
  Combo Text [PAction] |
  WindFury |
  Stealth |
  Secret Text [PAction] |
  CantTargetSpells |
  CantTargetHeroPower |
  Immune |
  Silence Text [TAction] |
  Silenced |
  UnknownMechanic Text
  deriving (Show)

data Target = Target PlayerTag Position deriving (Eq, Show)

data CardType = Spell | Minion | Weapon | Hero | HeroPower | Enchantment
  deriving (Eq, Show)

data CardState = Rushed Int | Green Int | Frozen | Dormant Int |
  Damaged Int | Buffed Int Int deriving (Eq, Show)

data CardClass =
  Druid |
  Hunter |
  Mage |
  Neutral |
  Paladin |
  Priest |
  Rogue |
  Shaman |
  Warlock |
  Warrior |
  Dream
  deriving (Eq, Show, Generic)

instance Hashable CardClass

allHeroClasses :: [CardClass]
allHeroClasses =
  [Druid,
   Hunter,
   Mage,
   Paladin,
   Priest,
   Rogue,
   Shaman,
   Warlock,
   Warrior]  

data GameState = GameState
  { firstPlayer :: Player
  , secondPlayer :: Player
  , playerTurn :: PlayerTag
  , rounds :: [[Action]]
  } deriving (Show)

initGame :: (MonadReader Env m) => (CardClass, [Card]) -> (CardClass, [Card]) ->
  m (Either Text GameState)
initGame (cc1, c1) (cc2, c2) = do
  p1 <- playerInit First cc1 c1
  p2 <- playerInit Second cc2 c2
  pure $ GameState <$>
    p1 <*>
    p2 <*>
    pure First <*>
    pure []

data Player = Player
  { deck :: [Card]
  , hand :: [Card]
  , board :: [Card]
  , hero :: Card
  , heroPower :: Card
  , weapon :: Maybe Card
  , mana :: Int
  , overloads :: Int
  } deriving (Show)

playerInit :: (MonadReader Env m) => PlayerTag -> CardClass -> [Card] ->
  m (Either Text Player)
playerInit pt cc cs = do
  env <- ask
  let h = maybe (Left "no more heros anymore") Right
        (Map.lookup cc (heros env))
  let hp = maybe (Left "no more heros anymore") Right
        (Map.lookup cc (heroPowers env))
  let (n,c) = if pt==Hearth.First then (3,[]) else (4, [theCoin env])
  pure $ Player <$> pure (drop n cs) <*> pure (take n cs <> c) <*> pure []
    <*> h <*> hp <*> pure Nothing <*> pure 10 <*> pure 0

data PlayerTag = First | Second deriving (Eq, Show)

data Action =
  StartTurn |
  Play Position |
  EndTurn
  deriving (Eq, Show)

data Position =
  Deck Int | Hand Int | Board Int | WeaponPos | HeroPos deriving (Eq, Show)

data Event =
  Destroy PlayerTag Position |
  Create PlayerTag Position |
  Mod PlayerTag Position (Card -> Card)

type Power = PlayerTag -> Position -> Event


text2mech :: Text -> Mechanic
text2mech t = case t of
  "BATTLECRY" -> Battlecry "" []
  "TAUNT" -> Taunt
  "CHARGE" -> Charge
  "DEATHRATTLE" -> Deathrattle "" []
  "RUSH" -> Rush
  "DIVINE_SHIELD" -> DivineShield
  "POISoNOUS" -> Poisonous
  "COMBO" -> Combo "" []
  "WINDFURY" -> WindFury
  "STEALTH" -> Stealth
  "SECRET" -> Secret "" []
  "CANT_BE_TARGETED_BY_SPELLS" -> CantTargetSpells
  "CANT_BE_TARGETED_BY_HERO_POWERS" -> CantTargetHeroPower
  x -> UnknownMechanic x

text2CardType :: Text -> Either Text CardType
text2CardType t = case t of
  "HERO" -> Right Hero
  "MINION" -> Right Minion
  "SPELL" -> Right Spell
  "WEAPON" -> Right Weapon
  "HERO_POWER" -> Right HeroPower
  "ENCHANTMENT" -> Right Enchantment
  x -> Left $ "Unknown cardType: " <> x

text2CardClass :: Text -> Either Text CardClass
text2CardClass "DRUID" = Right Druid
text2CardClass "HUNTER" = Right Hunter
text2CardClass "MAGE" = Right Mage
text2CardClass "NEUTRAL" = Right Neutral
text2CardClass "PALADIN" = Right Paladin
text2CardClass "PRIEST" = Right Priest
text2CardClass "ROGUE" = Right Rogue
text2CardClass "SHAMAN" = Right Shaman
text2CardClass "WARLOCK" = Right Warlock
text2CardClass "WARRIOR" = Right Warrior
text2CardClass "DREAM" = Right Dream
text2CardClass x = Left $ "Unknown CardClass: " <> x

cardClass2Text :: CardClass -> Text
cardClass2Text Druid = "DRUID"
cardClass2Text Hunter = "HUNTER"
cardClass2Text Mage = "MAGE"
cardClass2Text Neutral = "NEUTRAL"
cardClass2Text Paladin = "PALADIN"
cardClass2Text Priest = "PRIEST"
cardClass2Text Rogue = "ROGUE"
cardClass2Text Shaman = "SHAMAN"
cardClass2Text Warlock = "WARLOCK"
cardClass2Text Warrior = "WARRIOR"
cardClass2Text Dream = "DREAM"
```

JSON of cards
-------------

[HearthstoneJSON](https://hearthstonejson.com/docs/cards.html)

[cards](https://api.hearthstonejson.com/v1/latest/enUS/cards.json)

output
------

shuffle
-------

<img src='https://art.hearthstonejson.com/v1/render/latest/enUS/256x/GVG_005.png'><img src='https://art.hearthstonejson.com/v1/render/latest/enUS/256x/CS2_028.png'><img src='https://art.hearthstonejson.com/v1/render/latest/enUS/256x/ICC_838.png'>card set stats
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    number of cards          4,751
    attributes per card    1.120e1

attribute count
---------------

    id                       4,751
    set                      4,750
    name                     4,750
    dbfId                    4,750
    type                     4,750
    cardClass                4,745
    text                     4,020
    cost                     3,603
    artist                   2,388
    health                   2,147
    mechanics                1,942
    rarity                   1,939
    attack                   1,938
    collectible              1,615
    flavor                   1,597
    playRequirements           673
    race                       597
    elite                      465
    referencedTags             422
    entourage                  293
    howToEarnGolden            291
    howToEarn                  246
    targetingArrowText         123
    durability                 120
    hideStats                  102
    faction                     80
    collectionText              33
    overload                    29
    spellDamage                 24
    armor                       13
    multiClassGroup              9
    classes                      9
    questReward                  9

first card
----------

    set                  TB                 
    cardClass           NEUTRAL             
    cost                0                   
    health              0                   
    name                Golden Legendary    
    dbfId               52,424              
    rarity              LEGENDARY           
    id                  ART_BOT_Bundle_001  
    attack              0                   
    faction             ALLIANCE            
    type                MINION              

second card
-----------

    set                 TGT                 
    cardClass           MAGE                
    text                Deal $8 damage to a minion.
    flavor              It's on the rack next to ice lance, acid lance, and English muffin lance.
    playRequirements    Object (fromList [("REQ_MINION_TARGET",Number 0.0),("REQ_TARGET_TO_PLAY",Number 0.0)])
    cost                5                   
    collectible         Bool True           
    name                Flame Lance         
    dbfId               2,539               
    rarity              COMMON              
    id                  AT_001              
    type                SPELL               
    artist              Nutthapon Petchthai 

cards with no cardClass
-----------------------

0
-

    set                  TB                 
    entourage           Array [String "FB_LK_BossSetup001c",String "FB_LK_BossSetup001b",String "FB_LK_BossSetup001a"]
    name                Innkeeper Health Set
    dbfId               46,187              
    id                  FB_LK_BossSetup001  
    type                ENCHANTMENT         

1
-

    set                  TB                 
    text                Set the Lich King's health to 10000.
     <i>For huge groups only!</i>
    cost                0                   
    name                MASSIVE Difficulty  
    dbfId               46,184              
    id                  FB_LK_BossSetup001a 
    type                SPELL               

2
-

    set                  TB                 
    text                Set the Lich King's health to 3000.
    <i> Good for 10-25 players.</i>
    cost                0                   
    name                Heroic Difficulty   
    dbfId               46,185              
    id                  FB_LK_BossSetup001b 
    type                SPELL               

3
-

    set                  TB                 
    text                Set the Lich King's health to 1500.
    <i>Good for fewer than 10 players.</i>
    cost                0                   
    name                Normal Difficulty   
    dbfId               46,183              
    id                  FB_LK_BossSetup001c 
    type                SPELL               

4
-

    cost                0                   
    id                  PlaceholderCard     

5
-

    set                  TB                 
    text                Summon a random Imp.
    playRequirements    Object (fromList [("REQ_MINION_TARGET",Number 0.0)])
    cost                2                   
    name                Diabolical Powers   
    dbfId               42,156              
    id                  TB_SPT_DPromoHP     
    type                HERO_POWER          

set
---

    "BRM"                      221
    "CORE"                     220
    "CREDITS"                   87
    "EXPERT1"                  380
    "GANGS"                    257
    "GILNEAS"                  511
    "GVG"                      180
    "HERO_SKINS"                38
    "HOF"                       29
    "ICECROWN"                 296
    "KARA"                     220
    "LOE"                      254
    "LOOTAPALOOZA"             540
    "MISSIONS"                  36
    "NAXX"                     160
    "OG"                       218
    "TAVERNS_OF_TIME"           49
    "TB"                       623
    "TGT"                      207
    "UNGORO"                   224

type
----

    "ENCHANTMENT"              845
    "HERO"                     326
    "HERO_POWER"               370
    "MINION"                 1,815
    "SPELL"                  1,275
    "WEAPON"                   119

cardClass
---------

    "DEATHKNIGHT"               26
    "DREAM"                      6
    "DRUID"                    290
    "HUNTER"                   205
    "MAGE"                     205
    "NEUTRAL"                2,790
    "PALADIN"                  200
    "PRIEST"                   188
    "ROGUE"                    213
    "SHAMAN"                   188
    "WARLOCK"                  206
    "WARRIOR"                  228

rarity
------

    "COMMON"                   678
    "EPIC"                     283
    "FREE"                     193
    "LEGENDARY"                336
    "RARE"                     449

cost
----

    Just 0.0                   644
    Just 1.0                   545
    Just 2.0                   639
    Just 3.0                   522
    Just 4.0                   389
    Just 5.0                   331
    Just 6.0                   194
    Just 7.0                   111
    Just 8.0                    78
    Just 9.0                    46
    Just 10.0                   93
    Just 11.0                    5
    Just 12.0                    3
    Just 15.0                    1
    Just 20.0                    1
    Just 50.0                    1

health
------

    Just 0.0                     2
    Just 1.0                   275
    Just 2.0                   288
    Just 3.0                   290
    Just 4.0                   264
    Just 5.0                   237
    Just 6.0                   175
    Just 7.0                   100
    Just 8.0                    78
    Just 9.0                    28
    Just 10.0                   43
    Just 11.0                    5
    Just 12.0                   10
    Just 13.0                    1
    Just 14.0                    5
    Just 15.0                   16
    Just 16.0                    1
    Just 17.0                    1
    Just 18.0                    1
    Just 19.0                    1
    Just 20.0                   28
    Just 21.0                    1
    Just 22.0                    1
    Just 23.0                    1
    Just 24.0                    1
    Just 25.0                    7
    Just 26.0                    2
    Just 27.0                    1
    Just 28.0                    1
    Just 29.0                    1
    Just 30.0                  173
    Just 35.0                    3
    Just 40.0                   28
    Just 45.0                   13
    Just 50.0                   18
    Just 55.0                    1
    Just 60.0                   18
    Just 70.0                    6
    Just 75.0                    2
    Just 80.0                    6
    Just 85.0                    1
    Just 95.0                    2
    Just 99.0                    1
    Just 100.0                   5
    Just 200.0                   3
    Just 400.0                   1
    Just 1000.0                  1

attack
------

    Just 0.0                   115
    Just 1.0                   318
    Just 2.0                   415
    Just 3.0                   329
    Just 4.0                   252
    Just 5.0                   226
    Just 6.0                    97
    Just 7.0                    60
    Just 8.0                    45
    Just 9.0                    19
    Just 10.0                   28
    Just 11.0                    3
    Just 12.0                    5
    Just 13.0                    1
    Just 14.0                    1
    Just 15.0                    4
    Just 16.0                    1
    Just 17.0                    1
    Just 18.0                    1
    Just 19.0                    1
    Just 20.0                    4
    Just 21.0                    1
    Just 22.0                    1
    Just 23.0                    1
    Just 24.0                    1
    Just 25.0                    1
    Just 26.0                    1
    Just 27.0                    1
    Just 28.0                    1
    Just 29.0                    1
    Just 30.0                    3

text
----

    String "<b>Stealth</b>\nAt the end of your turn, summon a 1/1 Steward."         1
    String "Restore #8 Health."         1
    String "Summon one 5/5 Mithril Golem. <i>(Equip a weapon to upgrade.)</i>"         1
    String "<b>Battlecry:</b> Your emotes are now spoken in \"Radio Voice.\""         1
    String "After you cast a spell, summon a random minion of the same Cost."         1
    String "Swap George and Karl."         1
    String "Add 1 random Mage spell to your hand. @<i>(Play 2 Elementals to\160upgrade.)</i>"         1
    String "<i>This furbolg elder\ncalls arcane energies to her defense.</i>"         1
    String "<b>Taunt</b>. <b>Deathrattle:</b> Deal 2\ndamage to ALL characters."         1
    String "When this attacks, restore 4 Health to the hero of the player who buffed it."         1
    String "<b>Battlecry:</b>  Summon two 1/1 wolves."         1
    String "At end of your turn, deal 2 damage to the enemies opposite this minion."         1
    String "<b>Battlecry:</b> Give a friendly minion <b>Divine Shield</b>."         1
    String "Open a permanent portal that summons 3/2 Imps."         1
    String "This minion's Attack is equal to its Health."         1
    String "Whenever this minion takes damage, discard a\160random card."         1
    String "Deal $4 damage to a minion. If that kills it, add a copy of it to your\160hand."         1
    String "[x]<b>Battlecry:</b> The next Murloc\nyou play this turn costs\n\160Health instead of Mana."         1
    String "After you summon a minion, trigger its <b>Deathrattle</b> effect."         1
    String "Whenever a character is healed, gain +2 Attack."         2

mechanics
---------

    Array [String "SUMMONED"]         1
    Array [String "CHARGE",String "TAUNT"]         3
    Array [String "BATTLECRY",String "CHARGE"]         3
    Array [String "QUEST"]         9
    Array [String "COLLECTIONMANAGER_FILTER_MANA_EVEN",String "START_OF_GAME"]         1
    Array [String "CHOOSE_ONE"]        30
    Array [String "BATTLECRY",String "TAUNT"]        32
    Array [String "FREEZE",String "SPARE_PART"]         1
    Array [String "POISONOUS",String "STEALTH"]         3
    Array [String "BATTLECRY",String "ECHO"]         2
    Array [String "AUTOATTACK",String "TAUNT"]         2
    Array [String "LIFESTEAL",String "RUSH"]         2
    Array [String "DIVINE_SHIELD",String "RUSH"]         3
    Array [String "BATTLECRY",String "SPELLPOWER"]         2
    Array [String "CANT_ATTACK",String "INSPIRE"]         1
    Array [String "SECRET",String "ImmuneToSpellpower"]         3
    Array [String "DEATHRATTLE",String "MULTIPLY_BUFF_VALUE"]         1
    Array [String "DUNGEON_PASSIVE_BUFF"]        28
    Array [String "BATTLECRY",String "RUSH"]         3
    Array [String "AI_MUST_PLAY",String "FREEZE"]         2

playRequirements
----------------

    Object (fromList [("REQ_MINION_TARGET",Number 0.0),("REQ_TARGET_IF_AVAILABLE_AND_DRAGON_IN_HAND",Number 0.0),("REQ_LEGENDARY_TARGET",Number 0.0)])         1
    Object (fromList [("REQ_HERO_TARGET",Number 0.0)])         1
    Object (fromList [("REQ_TARGET_WITH_RACE",Number 20.0),("REQ_TARGET_TO_PLAY",Number 0.0),("REQ_FRIENDLY_TARGET",Number 0.0)])         1
    Object (fromList [("REQ_TARGET_TO_PLAY",Number 0.0)])        95
    Object (fromList [("REQ_TARGET_FOR_COMBO",Number 0.0)])         3
    Object (fromList [("REQ_MINION_TARGET",Number 0.0),("REQ_TARGET_TO_PLAY",Number 0.0),("REQ_TARGET_MIN_ATTACK",Number 5.0)])         1
    Object (fromList [("REQ_TARGET_WITH_RACE",Number 20.0),("REQ_TARGET_TO_PLAY",Number 0.0)])         1
    Object (fromList [("REQ_MINIMUM_ENEMY_MINIONS",Number 2.0)])         5
    Object (fromList [("REQ_MINION_TARGET",Number 0.0),("REQ_TARGET_IF_AVAILABLE",Number 0.0),("REQ_TARGET_WITH_DEATHRATTLE",Number 0.0),("REQ_FRIENDLY_TARGET",Number 0.0)])         2
    Object (fromList [("REQ_TARGET_IF_AVAILABLE",Number 0.0)])        16
    Object (fromList [("REQ_DRAG_TO_PLAY",Number 0.0)])         1
    Object (fromList [("REQ_MINIMUM_TOTAL_MINIONS",Number 2.0)])         1
    Object (fromList [("REQ_MINION_TARGET",Number 0.0),("REQ_TARGET_TO_PLAY",Number 0.0)])       152
    Object (fromList [("REQ_HERO_TARGET",Number 0.0),("REQ_TARGET_TO_PLAY",Number 0.0),("REQ_ENEMY_TARGET",Number 0.0)])         1
    Object (fromList [("REQ_TARGET_WITH_DEATHRATTLE",Number 0.0),("REQ_FRIENDLY_TARGET",Number 0.0)])         2
    Object (fromList [("REQ_MINION_TARGET",Number 0.0),("REQ_TARGET_FOR_COMBO",Number 0.0)])         2
    Object (fromList [("REQ_WEAPON_EQUIPPED",Number 0.0)])         5
    Object (fromList [("REQ_MINION_TARGET",Number 0.0)])        25
    Object (fromList [("REQ_TARGET_TO_PLAY",Number 0.0),("REQ_FRIENDLY_TARGET",Number 0.0)])         3
    Object (fromList [("REQ_TARGET_WITH_RACE",Number 15.0),("REQ_TARGET_TO_PLAY",Number 0.0)])         1

collectible
-----------

    Bool True                1,615

race
----

    String "BEAST"             222
    String "ALL"                 2
    String "TOTEM"               9
    String "DEMON"              66
    String "ELEMENTAL"          80
    String "PIRATE"             26
    String "DRAGON"             74
    String "MURLOC"             39
    String "ORC"                 1
    String "MECHANICAL"         78

elite
-----

    Bool True                  465

referencedTags
--------------

    Array [String "DIVINE_SHIELD"]        15
    Array [String "LIFESTEAL"]         8
    Array [String "POISONOUS",String "TAUNT"]         2
    Array [String "DEATHRATTLE",String "LIFESTEAL",String "RUSH"]         1
    Array [String "SPELLPOWER"]        10
    Array [String "ECHO"]         2
    Array [String "CHARGE"]        13
    Array [String "AUTOATTACK"]         9
    Array [String "CANT_ATTACK"]         1
    Array [String "DIVINE_SHIELD",String "TAUNT",String "WINDFURY"]         1
    Array [String "CHOOSE_ONE",String "JADE_GOLEM"]         1
    Array [String "CHARGE",String "TAUNT"]         5
    Array [String "ADAPT"]        16
    Array [String "DIVINE_SHIELD",String "LIFESTEAL",String "TAUNT",String "WINDFURY"]         1
    Array [String "SPARE_PART"]         5
    Array [String "DEATHRATTLE",String "DISCOVER"]         2
    Array [String "SECRET",String "STEALTH"]         1
    Array [String "JADE_GOLEM",String "TAUNT"]         1
    Array [String "DEATHRATTLE",String "SILENCE"]         1
    Array [String "SECRET"]        20

entourage
---------

    Array [String "TB_LethalSetup001a",String "TB_LethalSetup001b"]         1
    Array [String "EX1_543",String "UNG_919",String "EX1_116",String "FP1_010",String "GVG_110",String "FP1_013",String "ICC_314",String "OG_042",String "KAR_114",String "GVG_114"]         1
    Array [String "CS2_084",String "DS1_183",String "EX1_537",String "EX1_609",String "DS1_185",String "EX1_617",String "GVG_073",String "BRM_013",String "AT_056",String "UNG_910",String "ICC_049",String "LOOT_077"]         1
    Array [String "CS2_097",String "GVG_059",String "LOOT_286",String "GVG_061",String "OG_222",String "UNG_950",String "LOOT_500",String "LOOT_118",String "EX1_366",String "ICC_096"]         1
    Array [String "GILA_513",String "GILA_511",String "GILA_506"]         1
    Array [String "CS2_103",String "NEW1_011",String "EX1_414",String "CS2_146",String "AT_087",String "EX1_116",String "EX1_067",String "AT_070",String "UNG_099",String "AT_125"]         1
    Array [String "FB_ELO002a_copy",String "FB_ELO002b_copy",String "FB_ELO002c_copy"]         1
    Array [String "FP1_001",String "AT_030",String "LOE_019",String "EX1_012",String "EX1_059",String "FP1_004",String "EX1_616",String "FP1_024",String "tt_004"]         1
    Array [String "AT_003",String "AT_090",String "AT_006",String "AT_099",String "AT_127",String "ICC_833",String "AT_119",String "CFM_807",String "AT_008"]         1
    Array [String "CFM_313",String "LOE_018",String "AT_053",String "BRM_011",String "OG_026",String "ICC_081",String "EX1_248",String "EX1_258",String "OG_024",String "EX1_250",String "LOOT_064",String "ICC_090",String "AT_052",String "LOE_018",String "BRM_011",String "OG_026",String "EX1_567"]         1
    Array [String "OG_314",String "CS2_108",String "CS2_105",String "EX1_391",String "AT_064",String "EX1_408",String "KAR_028",String "GVG_052",String "CS2_114"]         1
    Array [String "EX1_298",String "GVG_114",String "ICC_314",String "FP1_013",String "UNG_933",String "EX1_572",String "OG_042",String "LOOT_380",String "AT_072",String "EX1_016",String "LOE_079",String "AT_127",String "GVG_056",String "LOOT_357"]         1
    Array [String "CS2_074",String "ICC_233",String "LOOT_542",String "GVG_023",String "ICC_221",String "UNG_823",String "ICC_850",String "CS2_233",String "ICC_240",String "GVG_022",String "ICC_097",String "LOOT_033",String "LOOT_389",String "ICC_096"]         1
    Array [String "CFM_062",String "CFM_815",String "UNG_015",String "CFM_062",String "EX1_383",String "GVG_085",String "UNG_808",String "ICC_314",String "ICC_912",String "OG_145",String "FP1_012",String "LOOT_383",String "EX1_032",String "KAR_061"]         1
    Array [String "OG_334",String "OG_096",String "OG_281",String "OG_162",String "OG_286",String "OG_283",String "OG_321",String "OG_131",String "OG_280",String "OG_255"]         1
    Array [String "EX1_162",String "EX1_531",String "tt_004",String "LOOT_136",String "CS2_237",String "AT_090",String "UNG_916",String "OG_045",String "OG_313",String "EX1_093",String "CS2_222"]         1
    Array [String "DS1_188",String "NEW1_018",String "EX1_536",String "GVG_043",String "GVG_119",String "CFM_325",String "CFM_337",String "LOOT_118",String "LOOT_222",String "LOOT_389",String "ICC_096"]         1
    Array [String "ICC_836",String "EX1_277",String "GVG_001",String "KAR_076",String "CFM_623",String "AT_009",String "GVG_003",String "LOOT_106",String "GIL_147",String "GIL_664"]         1
    Array [String "ICC_808",String "GVG_097",String "UNG_072",String "ICC_807",String "LOOT_131",String "UNG_801",String "UNG_848",String "LOOT_314",String "ICC_835",String "LOOT_137",String "UNG_108",String "FP1_012"]         1
    Array [String "CFM_639",String "CFM_753",String "CFM_305",String "GVG_063",String "CFM_650",String "CFM_685",String "CFM_853",String "BRM_028"]         1

targetingArrowText
------------------

    String "Trigger a Deathrattle."         2
    String "<b>Silence</b> a minion."         2
    String "Add a Golden copy to your hand."         1
    String "Restore 3 Health."         2
    String "Give +1 Attack."         1
    String "Restore a minion to full Health."         1
    String "Silence a minion with Deathrattle."         1
    String "Deal 1 damage.  Combo: 2 instead."         1
    String "Deal 3 damage."         3
    String "Change Attack to 1."         1
    String "Restore 5 Health."         1
    String "Steal a minion that has 2 or less Attack."         1
    String "Curse a minion."         1
    String "Destroy a minion."         3
    String "Return a minion to hand."         1
    String "Freeze an enemy."         2
    String "Restore 2 Health."         2
    String "Deal 4 damage."         2
    String "Grant <b>Taunt</b>."         1
    String "Return to your hand."         1

durability
----------

    Number 2.0                  51
    Number 4.0                  11
    Number 3.0                  34
    Number 5.0                   6
    Number 0.0                   1
    Number 1.0                   6
    Number 6.0                   6
    Number 8.0                   5

faction
-------

    String "ALLIANCE"           56
    String "HORDE"              24

collectionText
--------------

    String "<b>Divine Shield</b>, <b>Poisonous</b>"         1
    String "<b>Hero Power</b>\nDeal $3 damage."         2
    String "<b>Secret Deathrattle:</b> Deal 3 damage to all minions; or Give them +2/+2."         1
    String "Add 1 random Mage spell to your hand. <i>(Play 2 Elementals to\160upgrade.)</i>"         1
    String "<b>Echo</b>, <b>Rush</b>"         1
    String "Summon a 2/2 Spirit with <b>Taunt</b>. <i>(Restore 3 Health to upgrade.)</i>"         1
    String "<b>Lifesteal</b>, <b>Rush</b>"         1
    String "Destroy 1 random enemy minion.\n<i>(Play 3 <b>Deathrattle</b> cards to upgrade.)</i>"         1
    String "<b>Taunt</b>, <b>Poisonous</b>"         1
    String " <b>Battlecry and Deathrattle:</b> Summon a <b>Jade Golem</b>."         1
    String "Resurrect 2 different friendly minions. <i>(Cast 4 spells to upgrade.)</i>"         1
    String "Deal $4 damage. Summon a <b>Jade Golem</b>."         1
    String "<b>Poisonous</b>, <b>Rush</b>"         1
    String "Deal $2 damage to a minion. <i>(Gain 3 Armor to upgrade.)</i>"         1
    String "Summon a <b>Jade Golem</b>. Gain an empty Mana Crystal."         1
    String "<b>Stealth</b>\n<b>Deathrattle:</b> Summon a <b>Jade Golem</b>."         1
    String "<b>Choose One -</b> Summon a <b>Jade Golem</b>; or Shuffle 3 copies of this card into your deck."         1
    String "Summon a <b>Jade Golem</b>."         1
    String "Polymorph a random enemy minion."         1
    String "<b>Battlecry:</b> Summon a <b>Jade\160Golem</b>."         1

overload
--------

    Number 1.0                  12
    Number 10.0                  1
    Number 5.0                   1
    Number 3.0                   4
    Number 2.0                  11

workflow
========

    stack build --exec "$(stack path --local-install-root)/bin/hheartstone" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i src/Hearth.lhs -t markdown -o other/api.md --filter pandoc-include --mathjax" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/readme_.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch
