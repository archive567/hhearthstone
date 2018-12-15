{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Hearth.Card where

import Hearth.Enum
import NumHask.Prelude hiding (First)
import Control.Lens hiding (Magma)
import Data.Generics.Product (field)
import qualified Data.HashMap.Strict as Map

newtype Id = Id { unid :: Text } deriving (Eq, Show, Generic, Hashable)

-- | A Hearthstone card used in a game
data Card = Card
  { id :: Id
  , isGreen :: Bool
  , isRush :: Bool
  , isFrozen :: Bool
  , isSilenced :: Bool
  , oattack :: Int
  , ohealth :: Int
  , ocost :: Int
  , odurability :: Int
  , buffs :: [Buff]
  , damages :: Int
  , durabilityUsed :: Int
  , dormant :: Maybe Int
  , cardtype :: CardType
  , powers :: [Power]
  } deriving (Show, Eq, Generic)

-- | card buffs
data Buff = Buff
  { battack :: Int
  , bhealth :: Int
  , bcost :: Int
  , bdurability :: Int
  } deriving (Show, Eq, Generic)

-- | SCard is a standard card before any modifications
data SCard = SCard
  { scost :: Int
  , sattack :: Int
  , shealth :: Int
  , sid :: Id
  , name :: Text
  -- , mechanics :: [Mechanic]
  , cardType :: CardType
  , cardClass :: CardClass
  , race :: Maybe Text
  , spellDamage :: Maybe Int
  , overloaded :: Maybe Int
  , sdurability :: Int
  , targetText :: Maybe Text
  , entourage :: Maybe [Id]
  , cardText :: Text
  } deriving (Show, Generic)

instance Eq SCard where
  (==) a b = sid a == sid b

look :: CardMap -> Card -> (SCard -> a) -> a -> a
look s c f d = maybe d f (Map.lookup (c ^. field @"id") (cardMap s))

-- | the set of all standard cards, as a HashMap
newtype CardMap = CardMap { cardMap :: Map.HashMap Id SCard}

-- | a fresh Card
fresh :: CardMap -> CardPowerSet -> Id -> Card
fresh s ps i = Card i False False False False a h c d [] 0 0 Nothing t p
  where
    a = maybe 0 sattack (Map.lookup i (cardMap s))
    h = maybe 0 shealth (Map.lookup i (cardMap s))
    c = maybe 0 scost (Map.lookup i (cardMap s))
    d = maybe 0 sdurability (Map.lookup i (cardMap s))
    t = maybe Minion cardType (Map.lookup i (cardMap s))
    p = maybe [] identity (Map.lookup i (cardPowerMap ps))

freshCard :: CardMap -> SCard -> Card
freshCard s sc = fresh s defaultCardPowerSet (sid sc)

-- | card powers
data Power = ManaThisTurn Int | NoPower deriving (Show, Eq, Generic)

newtype CardPowerSet = CardPowerSet { cardPowerMap :: Map.HashMap Id [Power]}

defaultCardPowerSet :: CardPowerSet
defaultCardPowerSet = CardPowerSet $ Map.fromList [(Id "GAME_005", [ManaThisTurn 1])]

cost :: Card -> Int
cost c =
  ocost c +
  sum ((^. field @"bcost") <$> (c ^. field @"buffs"))

attack :: Card -> Int
attack c =
  oattack c +
  sum ((^. field @"battack") <$> (c ^. field @"buffs"))

health :: Card -> Int
health c =
  ohealth c +
  sum ((^. field @"bhealth") <$> (c ^. field @"buffs"))

durability :: Card -> Int
durability c =
  odurability c -
  (c ^. field @"durabilityUsed")

-- | evaluation
data CardStats = CardStats
  { cardCount :: Double
  , costs :: Double
  , attacks :: Double
  , healths :: Double
  , durabilitys :: Double
  } deriving Show

instance Magma (Sum CardStats) where
  (Sum x) `magma` (Sum y) = Sum (plus' x y)
    where
      plus' :: CardStats -> CardStats -> CardStats
      plus' (CardStats n c a h d) (CardStats n' c' a' h' d') =
        CardStats (n+n') (c+c') (a+a') (h+h') (d+d')

instance Unital (Sum CardStats) where
  unit = Sum (CardStats 0 0 0 0 0)

instance Associative (Sum CardStats)

instance Commutative (Sum CardStats)

instance Invertible (Sum CardStats) where
  inv (Sum (CardStats n c a h d)) = Sum (CardStats (negate n) (negate c) (negate a) (negate h) (negate d))

instance MultiplicativeAction CardStats Double where
  (.*) (CardStats a b c d e) x = CardStats (a*x) (b*x) (c*x) (d*x) (e*x)
  (*.) = flip (.*)

cardStat :: Card -> CardStats
cardStat c =
  CardStats
  1
  (fromIntegral $ cost c)
  (fromIntegral $ attack c)
  (fromIntegral $ health c)
  (fromIntegral $ durability c)

defaultCardsEval :: CardStats
defaultCardsEval = CardStats
  1
  1
  1
  1
  1

evalCards :: CardStats -> CardStats -> Double
evalCards (CardStats n' c' a' h' d') (CardStats n c a h d) =
  sum
  [ n'*n
  , c'*c
  , a'*a
  , h'*h
  , d'*d
  ]

startCard :: Card -> Card
startCard c =
  (field @"isFrozen" .~ False) .
  (field @"isGreen" .~ (attack c > 0)) $ c

endCard :: Card -> Card
endCard =
  field @"isGreen" .~ False

hit :: (Card, Card) -> (Card, Card)
hit (off, def) = (off', def')
  where
    off' = off & field @"damages" %~ (+ attack def)
    def' = def & field @"damages" %~ (+ attack off)

dead :: Card -> Bool
dead c = health c <= 0

removeC :: Int -> [Card] -> [Card]
removeC x xs = take x xs <> drop (x+1) xs

insertC :: Int -> Card -> [Card] -> [Card]
insertC x c xs = take x xs <> [c] <> drop x xs

replaceC :: Int -> Card -> [Card] -> [Card]
replaceC x c xs = take x xs <> [c] <> drop (x+1) xs
