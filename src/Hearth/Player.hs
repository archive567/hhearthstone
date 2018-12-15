{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Hearth.Player where

import Control.Lens hiding (Magma)
import Data.Generics.Product (field)
import Data.List hiding (sum)
import GHC.Show
import Hearth.Enum
import Hearth.Card
import NumHask.Prelude hiding (First, replace)

-- | Player
data Player = Player
  { deck :: [Card]
  , hand :: [Card]
  , board :: [Card]
  , hero :: Card
  , heroPower :: Card
  , weapon :: Maybe Card
  , mana :: Int
  , manaLeft :: Int
  , overload :: Int
  , fatigue :: Int
  } deriving (Show, Generic)

data PlayerTag = First | Second deriving (Eq, Show)

data Position =
  Deck Int | Hand Int | Board Int | HeroPos | HeroPowerPos | WeaponPos
  deriving (Eq, Show)

data Play =
  ToBoard Position Int |
  Attack Position Position |
  Cast Position |
  EndButton
  deriving (Eq, Show)

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
  { evalDeck :: CardStats
  , evalHand :: CardStats
  , evalBoard :: CardStats
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
  sum
  [ evalCards d d'
  , evalCards h h'
  , evalCards b b'
  , hh'*hh
  , m*m'
  ]

startPlayer :: Int -> Player -> Player
startPlayer r p = Player d h b' hero' hp' w' m' ml' o' f
  where
    (Player d h b hero' hp w _ _ o f) = deal p
    m' = min 10 r -- FIXME permanent mana buffs
    ml' = max 0 (m' - o)
    o' = 0
    b' = startCard <$> b
    w' = startCard <$> w
    hp' = startCard hp

endPlayer :: Player -> Player
endPlayer (Player d h b hero' hp w m ml o f) =
  Player d h b' hero' hp' w' m ml o f
  where
    b' = endCard <$> b
    w' = endCard <$> w
    hp' = endCard hp

-- | partial
-- FIXME:
move :: Position -> Position -> Player -> Player
move pos@(Hand x) (Board x') p
  | x >= length (p ^. field @"hand") = p
  | otherwise =
    p &
    field @"hand" %~ removeC x &
    field @"board" %~ insertC x' (card p pos)

remove :: Position -> Player -> Player
remove (Deck i) p = p & field @"deck" %~ removeC i
remove (Hand i) p = p & field @"hand" %~ removeC i
remove (Board i) p = p & field @"board" %~ removeC i
remove HeroPos p = p
remove HeroPowerPos p = p
remove WeaponPos p = p & field @"weapon" .~ Nothing

replace :: Card -> Position -> Player -> Player
replace c (Deck i) p = p & field @"deck" %~ replaceC i c
replace c (Hand i) p = p & field @"hand" %~ replaceC i c
replace c (Board i) p = p & field @"board" %~ replaceC i c
replace c HeroPos p = p & field @"hero" .~ c
replace c HeroPowerPos p = p & field @"heroPower" .~ c
replace c WeaponPos p = p & field @"weapon" .~ Just c

insert :: Card -> Position -> Player -> Player
insert c (Deck i) p = p & field @"deck" %~ insertC i c
insert c (Hand i) p = p & field @"hand" %~ insertC i c
insert c (Board i) p = p & field @"board" %~ insertC i c
insert _ HeroPos p = p
insert _ HeroPowerPos p = p
insert c WeaponPos p = p & field @"weapon" .~ Just c

deal :: Player -> Player
deal (Player d h b hero' hp w m ml o f) = Player d' h' b hero'' hp w m ml o f' where
    (hero'',d',h',f') = case d of
      [] -> (hero' & (field @"damages" %~ (+(f+1))), [], h, f+1)
      (x:xs) -> (hero',xs,h<>[x],0)

attacks' :: (Player, Position) -> (Player, Position) -> (Player, Player)
attacks' (p, pos) (p', pos') = (p1,p1')
  where
    (off,def) = hit (card p pos, card p' pos')
    p1 = p & bool (remove pos) (replace off pos) (dead off)
    p1' = p' & bool (remove pos') (replace def pos') (dead def)

chargeMana :: Position -> Player -> Player
chargeMana pos p = p & field @"manaLeft" %~ (\x -> x - (cost (card p pos)))

-- FIXME Maybe Card
card :: Player -> Position -> Card
card p (Deck i) = p ^. field @"deck" & (!!i)
card p (Board i) = p ^. field @"board" & (!!i)
card p (Hand i) = p ^. field @"hand" & (!!i)
card p HeroPos = p ^. field @"hero"
card p HeroPowerPos = p ^. field @"heroPower"
-- card p WeaponPos = p ^. field @"weapon"

findPlays :: Player -> Player -> [Play]
findPlays p p' =
  hand2BoardPlays p <>
  boardPlays p p' <>
  spellPlays p <>
  [EndButton]

hand2BoardPlays :: Player -> [Play]
hand2BoardPlays p =
  ToBoard . Hand
  <$> findIndices
    (canPlay2Board (p ^. field @"manaLeft"))
    (p ^. field @"hand")
  <*> take (p^.field @"board" & length & (+1)) [0..]

spellPlays :: Player -> [Play]
spellPlays p =
  Cast . Hand
  <$> findIndices
    (canPlaySpell (p ^. field @"manaLeft"))
    (p ^. field @"hand")

canPlay2Board :: Int -> Card -> Bool
canPlay2Board m c =
  c^.field @"cardtype" == Minion && cost c <= m

canPlaySpell :: Int -> Card -> Bool
canPlaySpell m c =
  c^.field @"cardtype" == Spell && cost c <= m

boardPlays :: Player -> Player -> [Play]
boardPlays p p' =
  (Attack <$> rushes <*> boardTargets) <>
  (Attack <$> greens <*> greenTargets)
  where
  rushes = Hand <$> findIndices
    (\c -> c ^. field @"isRush")
    (p ^. field @"board")
  greens = Hand <$> findIndices
    (\c -> c ^. field @"isGreen")
    (p ^. field @"board")
  boardTargets =
    Board <$> take (length $ p' ^. field @"board") [0..]
  greenTargets =
    boardTargets <> [HeroPos]
