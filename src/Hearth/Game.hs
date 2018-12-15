{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Hearth.Game where

import Control.Lens hiding (Magma)
import Control.Monad.Primitive
import Data.Generics.Product (field)
import Data.List hiding (sum)
import GHC.Show
import Hearth.Card
import Hearth.Enum
import Hearth.Player
import NumHask.Prelude hiding (First, cast)
import System.Random.MWC
import qualified Data.HashMap.Strict as Map

data Env = Env
  { gen :: Gen (PrimState IO)
  , allCards :: CardMap
  , collectibles :: CardMap
  , heros :: Map.HashMap CardClass SCard
  , heroPowers :: Map.HashMap CardClass SCard
  , theCoin :: SCard
  }

playerInit :: (MonadReader Env m) => PlayerTag -> CardClass -> [Card] ->
  m (Either Text Player)
playerInit pt cc cs = do
  env <- ask
  let h = maybe (Left "no more heros anymore") Right
        (Map.lookup cc (heros env))
  let hp = maybe (Left "no more heros anymore") Right
        (Map.lookup cc (heroPowers env))
  let g scard = fresh (allCards env) defaultCardPowerSet (sid scard)
  let (n,c,ml) = if pt==First then (4,[],1) else (4, [g (theCoin env)],0)
  pure $ Player <$> pure (drop n cs) <*> pure (take n cs <> c) <*> pure []
    <*> fmap g h <*> fmap g hp <*> pure Nothing <*> pure 1 <*> pure ml <*> pure 0 <*> pure 0

data Game = Game
  { firstp :: Player
  , secondp :: Player
  , round :: Int
  , turn :: PlayerTag
  } deriving (Show, Generic)

act :: Lens' Game Player
act =
  lens
  (\g -> bool (secondp g) (firstp g) (turn g == First))
  (\g p -> bool (g {secondp=p}) (g {firstp=p}) (turn g == First))

opp :: Lens' Game Player
opp =
  lens
  (\g -> bool (secondp g) (firstp g) (turn g == Second))
  (\g p -> bool (g {secondp=p}) (g {firstp=p}) (turn g == Second))

initGame :: (MonadReader Env m) => (CardClass, [Card]) -> (CardClass, [Card]) ->
  m (Either Text Game)
initGame (cc1, c1) (cc2, c2) = do
  p1 <- playerInit First cc1 c1
  p2 <- playerInit Second cc2 c2
  pure $ Game <$>
    p1 <*>
    p2 <*>
    pure 1 <*>
    pure First

data GameStats = GameStats
  { firstStats :: PlayerStats
  , secondStats :: PlayerStats
  , turnStats :: PlayerTag
  } deriving (Show)

gameStats :: Game -> GameStats
gameStats g =
  GameStats
  (g ^. field @"firstp" & playerStats)
  (g ^. field @"secondp" & playerStats)
  (g ^. field @"turn")

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
  sum
  [ evalPlayer f f'
  , negate (evalPlayer s s')
  ] +
  case t' of
    First -> -1 * t
    Second -> 1 * t

-- | turn mechanics
nextTurn :: Game -> Game
nextTurn g = g' & act %~ startPlayer (g'^.field @"round")
  where
    g' = g & act %~ endPlayer & flipTurn

flipTurn :: Game -> Game
flipTurn g =
  case g^.field @"turn" of
    First -> g & field @"turn" .~ Second
    Second -> g & field @"turn" .~ First & field @"round" %~ (+1)

play :: Play -> Game -> Game
play EndButton g = nextTurn g
play (ToBoard pos i) g = g & act %~ move pos (Board i) . chargeMana pos
play (Attack pos pos') g = g & act .~ off & opp .~ def
  where
  (off,def) = attacks' (g^.act,pos) (g^.opp,pos')
play (Cast pos) g = cast pos g

cast :: Position -> Game -> Game
cast pos g = bool g
  ( applyPowers (c^.field @"powers") g &
    act %~ remove pos . chargeMana pos)
  canCast
  where
    c = card (g ^. act) pos
    canCast = c^.field @"cardtype" == Spell

applyPower :: Power -> Game -> Game
applyPower (ManaThisTurn m) g = g & (act . field @"manaLeft") %~ (+m)
applyPower _ g = g

applyPowers :: [Power] -> Game -> Game
applyPowers ps g = foldl' (flip applyPower) g ps

-- | finding plays
plays :: Game -> [Play]
plays g = findPlays (g^.act) (g^.opp)

