\begin{code}
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

\end{code}
