{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}

module Hearth
  ( module Hearth.Card
  , module Hearth.Enum
  , module Hearth.Player
  , module Hearth.Game
  , module Hearth.Format
  , module Hearth.Json
  , module Control.Lens
  , field
  ) where

import Control.Lens hiding (Magma, magma, (<.>), unsnoc, uncons, to, from, Strict)
import Data.Generics.Product (field)
import Hearth.Card
import Hearth.Enum
import Hearth.Format
import Hearth.Game
import Hearth.Json
import Hearth.Player


-- | Actions
-- https://github.com/jleclanche/fireplace/wiki/Actions

{-
data Action = ActionGame GameAction | ActionTarget TargetedAction [Target]

data TargetedAction = Buff Enchantment | Bounce | Counter | Damage Int | Deathrattle | Destroy | Discard | Draw | ForceDraw Match | FullHeal | GainArmor | GainMana Int | Give EvalCard | Hit Int Source | ManaThisTurn Int | Mill Int | Morph EvalCard | Freeze | FillMana Int | Reveal | SetTag [GameTags] | Silence | Summon EvalCard | Shuffle EvalCard | Steal | UnlockOverload

data GameAction = Attack Source Target | BeginTurn PlayerTag | Death Target | EndTurn PlayerTag | Play Card Target Choose

-}

