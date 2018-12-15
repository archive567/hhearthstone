{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Hearth.Format where

import Protolude hiding ((%), First)
import Data.Aeson
import qualified Data.Text as Text
import Formatting
import Data.Scientific
import Hearth.Enum
import Hearth.Game
import Hearth.Player
import Hearth.Card
import Data.Generics.Product (field)
import Control.Lens

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

art' :: Int -> Text -> Text
art' w d =
  "<img src='https://art.hearthstonejson.com/v1/render/latest/enUS/256x/" <>
  d <> ".png' width='" <> show w <> "%'>"

artGlow' :: Int -> Bool -> Text -> Text
artGlow' w g d =
  "<img src='https://art.hearthstonejson.com/v1/render/latest/enUS/256x/"
  <> d
  <> ".png' style='width:"
  <> show w
  <> "%"
  <> bool "'" ";box-shadow: 0px 0px 20px 10px #3f4;'" g
  <> ">"

renderCard :: Card -> Text
renderCard c = artGlow' 15 (c ^.field @"isGreen") (unid $ c^.field @"id")

renderCards :: [Card] -> Text
renderCards cs = Text.intercalate " " $ renderCard <$> cs

renderPlayerStats :: CardMap -> PlayerTag -> Player -> Text
renderPlayerStats s t p = Text.intercalate "\n"
  [ bool "second" "first" (t==First)
    <> " player: "
    <> show (look s (p^.field @"hero") cardClass Mage)
    <> " h:" <> show (health (p^.field @"hero"))
  , " mana: "
    <> show (p^.field @"mana")
    <> "/"
    <> show (p^.field @"manaLeft")
  , "deck:" <> show (length $ p^.field @"deck")
  ]

renderGame :: CardMap -> Game -> Text
renderGame s game = Text.intercalate "<br>\n"
    [ renderPlayerStats s First (game^.field @"firstp")
    , game ^. field @"firstp" ^. field @"hand" & renderCards
    , game ^. field @"firstp" ^. field @"board" & renderCards
    , game ^. field @"secondp" ^. field @"board" & renderCards
    , game ^. field @"secondp" ^. field @"hand" & renderCards
    , renderPlayerStats s Second (game^.field @"secondp")
    , "turn: " <> show (game ^. field @"turn") <> " round: " <>
      show (game ^. field @"round") <> " eval: " <>
      show (evalGame defaultGameEval (gameStats game))
    , "plays: " <> show (plays game)
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

renderPlayerShort :: CardMap -> PlayerTag -> Player -> Text
renderPlayerShort s t p = Text.intercalate "\n" 
  [ bool "second" "first" (t==First)
    <> " player: "
    <> show (look s (p^.field @"hero") cardClass Mage)
  , "mana: "
    <> show (p^.field @"manaLeft")
    <> "/"
    <> show (p^.field @"mana")
  , "deck left: " <> show (length $ p^.field @"deck")
  , "board: " <> show ((\x -> look s x name "error") <$> p^.field @"board")
  , "hand : " <> show ((\x -> look s x name "error") <$> p^.field @"hand")
  ]

renderGameShort :: CardMap -> Game -> Text
renderGameShort s g = Text.intercalate "\n"
  [
    "turn: " <> show (g ^. field @"turn") <> " round: " <>
    show (g ^. field @"round") <> " eval: " <> show (evalGame defaultGameEval (gameStats g))
  , g ^. field @"firstp" & renderPlayerShort s First
  , g ^. field @"secondp" & renderPlayerShort s Second
  ]

