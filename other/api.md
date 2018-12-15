``` {.sourceCode .literate .haskell}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hearth where

import NumHask.Prelude hiding (First)
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
  , overloaded :: Maybe Int
  , durability :: Maybe Int
  , targetText :: Maybe Text
  , entourage :: Maybe [Id]
  , cardText :: Text
  } deriving (Show)

instance Eq Card where
  (==) a b = id a == id b

data CardStats = CardStats
  { cardCount :: Double
  , costs :: Double
  , attacks :: Double
  , healths :: Double
  , overloadeds :: Double
  , durabilitys :: Double
  } deriving Show

instance Magma (Sum CardStats) where
  (Sum x) `magma` (Sum y) = Sum (plus' x y)
    where
      plus' :: CardStats -> CardStats -> CardStats
      plus' (CardStats n c a h o d) (CardStats n' c' a' h' o' d') =
        CardStats (n+n') (c+c') (a+a') (h+h') (o+o') (d+d')

instance Unital (Sum CardStats) where
  unit = Sum (CardStats 0 0 0 0 0 0)

instance Associative (Sum CardStats)

instance Commutative (Sum CardStats)

instance Invertible (Sum CardStats) where
  inv (Sum (CardStats n c a h o d)) = Sum (CardStats (negate n) (negate c) (negate a) (negate h) (negate o) (negate d))

instance MultiplicativeAction CardStats Double where
  (.*) (CardStats a b c d e f) x = CardStats (a*x) (b*x) (c*x) (d*x) (e*x) (f*x)
  (*.) = flip (.*)

instance MultiplicativeAction EvalCardConfig Double where
  (.*) (EvalCardConfig a b c d) x = EvalCardConfig (a*x) (b*x) (c*x) (d*x)
  (*.) = flip (.*)

cardStat :: Card -> CardStats
cardStat c =
  CardStats
  1
  (fromIntegral $ cost c)
  (fromIntegral $ attack c)
  (fromIntegral $ health c)
  (maybe 0 fromIntegral (overloaded c))
  (maybe 0 fromIntegral (durability c))

data EvalCardConfig = EvalCardConfig
  { evalCount :: Double
  , evalCost :: Double
  , evalAttack :: Double
  , evalHealth :: Double
  }

defaultCardsEval :: EvalCardConfig
defaultCardsEval = EvalCardConfig
  1
  1
  1
  1

evalCards :: EvalCardConfig -> CardStats -> Double
evalCards (EvalCardConfig n' c' a' h') (CardStats n c a h _ _) =
  sum [(n'*n), (c'*c), (a'*a), (h'*h)]

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

data GameStats = GameStats
  { firstStats :: PlayerStats
  , secondStats :: PlayerStats
  , turnStats :: PlayerTag
  } deriving (Show)

gameStats :: GameState -> GameStats
gameStats g = GameStats (playerStats $ firstPlayer g) (playerStats $ secondPlayer g) (playerTurn g)

data EvalGameConfig = EvalGameConfig
  { evalFirst :: EvalPlayerConfig
  , evalSecond :: EvalPlayerConfig
  , evalTurn :: Double
  }

defaultGameEval :: EvalGameConfig
defaultGameEval = EvalGameConfig
  defaultPlayerEval
  defaultPlayerEval
  0

evalGame :: EvalGameConfig -> GameStats -> Double
evalGame (EvalGameConfig f s t) (GameStats f' s' t') =
  sum [(evalPlayer f f'), (negate $ evalPlayer s s')] +
  case t' of
    First -> -1 * t
    Second -> 1 * t

data Player = Player
  { deck :: [Card]
  , hand :: [Card]
  , board :: [Card]
  , hero :: Card
  , heroPower :: Card
  , weapon :: Maybe Card
  , mana :: Int
  , overload :: Int
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
    <*> h <*> hp <*> pure Nothing <*> pure 0 <*> pure 0

data PlayerStats = PlayerStats
  { deckStats :: CardStats
  , handStats :: CardStats
  , boardStats :: CardStats
  , manaStat :: Double
  , heroHealth :: Double
  } deriving (Show)

playerStats :: Player -> PlayerStats
playerStats p =
  PlayerStats
  (sum (cardStat <$> deck p))
  (sum (cardStat <$> hand p))
  (sum (cardStat <$> board p))
  (fromIntegral $ mana p - overload p)
  (fromIntegral $ health (hero p))

data EvalPlayerConfig = EvalPlayerConfig
  { evalDeck :: EvalCardConfig
  , evalHand :: EvalCardConfig
  , evalBoard :: EvalCardConfig
  , evalMana :: Double
  , evalHeroHealth :: Double
  }

defaultPlayerEval :: EvalPlayerConfig
defaultPlayerEval = EvalPlayerConfig
  ((0.2 :: Double) *. defaultCardsEval)
  ((0.6 :: Double) *. defaultCardsEval)
  ((1 :: Double) *. defaultCardsEval)
  1
  1

evalPlayer :: EvalPlayerConfig -> PlayerStats -> Double
evalPlayer (EvalPlayerConfig d h b m hh) (PlayerStats d' h' b' m' hh') =
  sum [(evalCards d d'), (evalCards h h'), (evalCards b b'), (hh'*hh), (m*m')]

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
