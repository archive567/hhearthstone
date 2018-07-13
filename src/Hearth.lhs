\begin{code}
module Hearth where

import Protolude

data Card = Card
  { cost :: Int
  , attack :: Int
  , health :: Int
  , name :: Text
  , mechanics :: [Mechanics]
  , cardType :: CardType
  , cardState :: [CardState]
  }

data Mechanics =
  Charge |
  Battlecry [Target -> [Event]] |
  Deathrattle [Target -> [Event]] |
  Powers [Position -> Target -> [Event]] |
  Rush

data Target = Target PlayerTag Position

data CardType = Spell | Minion | Weapon | Hero | HeroPower | Buffs deriving (Eq, Show)

data CardState = Rushed Int | Green Int | Frozen | Dormant Int | Damaged Int | Buffed Int Int

data State = State
  { firstPlayer :: Player
  , secondPlayer :: Player
  , playerTurn :: PlayerTag
  , rounds :: [[Action]]
  }

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

data Player = Player
  { deck :: [Card]
  , hand :: [Card]
  , hero :: Card
  , weapon :: Maybe Card
  , mana :: Int
  , overloads :: Int
  }
\end{code}
