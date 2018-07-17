\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Hearth.Format where

import Protolude hiding ((%))
import Data.Aeson
import qualified Data.Text as Text
import Formatting
import Data.Scientific
import Hearth

-- | fixed precision for a Scientific
prec :: Int -> Format r (Scientific -> r)
prec n = scifmt Exponent (Just n)

int2Sci :: (Integral a) => a -> Scientific
int2Sci n = scientific (fromIntegral n) 0

-- | place markdown backtics around text
code :: [Text] -> Text
code cs = "\n```\n" <> Text.intercalate "\n" cs <> "\n```\n"

h2 :: Text -> Text
h2 h = h <> "\n---\n"

art :: Text -> Text
art d =
  "<img src='https://art.hearthstonejson.com/v1/render/latest/enUS/256x/" <>
  d <> ".png'>"

renderGame :: GameState -> Text
renderGame game = Text.intercalate "\n" ts where
  ts =
    [ "turn: " <> show (playerTurn game)
    , "player 1: " <> name (hero (firstPlayer game)) <>
      " mana: " <> show (mana (firstPlayer game)) <>
      " deck left: " <> show (length $ deck (firstPlayer game))
    , " hand: " <> Text.intercalate " " (art . id <$>
                                          hand (firstPlayer game))
    , " board: " <> Text.intercalate " " (art . id <$>
                                          board (firstPlayer game))
    , "player 2: " <> name (hero (secondPlayer game)) <>
      " mana: " <> show (mana (secondPlayer game)) <>
      " deck left: " <> show (length $ deck (secondPlayer game))
    , " hand: " <> Text.intercalate " " (art . id <$>
                                          hand (secondPlayer game))
    , " deck: " <> Text.intercalate " " (art . id <$>
                                          board (secondPlayer game))
    ]

fK :: Text -> Text -> Text
fK = sformat ((right 20 ' ' %. stext) % (left 10 ' ' %. stext))

fKR :: Text -> Text -> Text
fKR = sformat ((right 20 ' ' %. stext) % (right 20 ' ' %. stext))

fI :: (Integral a, Buildable a) => a -> Text
fI = sformat commas

fV :: Int -> Int -> Value -> Text
fV i _ (String x) = sformat (left i ' ' %. stext) x
fV i p (Number x) = sformat (right i ' ' %. stext) (fS p x)
fV i _ x = sformat (right i ' ' %. stext) (show x)

fS :: Int -> Scientific -> Text
fS p x = case floatingOrInteger x of
  Left _ -> sformat (prec p) x
  Right (i :: Int) -> sformat commas i

\end{code}
