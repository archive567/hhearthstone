{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Hearth.Json where

import Control.Lens hiding (Magma)
import Data.Aeson
import Data.Scientific
import Hearth.Card
import Hearth.Enum
import Hearth.Format
import Hearth.Game
import Protolude hiding (from, to)
import System.Random.MWC
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as V

filt :: [(Text, Value -> Bool)] -> [Object] -> [Object]
filt [] xs = xs
filt ((k,b):fs) xs = filt fs $ filter (maybe False b . Map.lookup k) xs

unarray :: Value -> [Value]
unarray = \case
  Array a -> V.toList a
  _ -> []

unobject :: Value -> Object
unobject = \case
  Object a -> a
  _ -> Map.empty

unstring :: Value -> Text
unstring = \case
  String a -> a
  _ -> ""

unnumber :: Value -> Maybe Scientific
unnumber = \case
  Number a -> Just a
  _ -> Nothing

unint :: Value -> Maybe Int
unint = \case
  Number a -> toBoundedInteger a
  _ -> Nothing

oint :: Object -> Text -> Either Text Int
oint o k =
  maybe (Left ("no "<> k))
      (maybe (Left $ "bad number conversion for " <> k) Right)
      (unint <$> Map.lookup k o)

mint :: Object -> Text -> Maybe Int
mint o k = join $ unint <$> Map.lookup k o

otext :: Object -> Text -> Either Text Text
otext o k =
  maybe (Left ("no "<> k))
      Right
      (unstring <$> Map.lookup k o)

mtext :: Object -> Text -> Maybe Text
mtext o k = unstring <$> Map.lookup k o

mid :: Object -> Maybe Text
mid o = mtext o "id"

otexts :: Object -> Text -> Maybe [Text]
otexts o k = fmap unstring <$> (unarray <$> Map.lookup k o)

omech :: Object -> [Text]
omech o = maybe [] identity
  (fmap unstring . unarray <$> Map.lookup "mechanics" o)

getJsonCards :: IO [Object]
getJsonCards = do
  t <- B.readFile "other/cards.json"
  pure $ maybe [] (fmap unobject . unarray) (decode t)

countKeys :: [Object] -> Map.HashMap Text Integer
countKeys = foldl' u Map.empty where
  u x a = Map.unionWith (+) x (Map.map (const 1) a)

countAllValues :: [Object] -> Map.HashMap (Text, Value) Integer
countAllValues = foldl' u Map.empty where
  u x a = Map.unionWith (+) x
    (Map.fromList $ (\(k,v) -> ((k,v),1)) <$>
     Map.toList a)

countValues :: Text -> [Object] -> [(Value, Integer)]
countValues k' xs =
  (\((_ , v),x) -> (v, x)) <$>
  Map.toList (Map.filterWithKey (\(k,_) _ -> k ==k')
   (countAllValues xs))

attrF :: Int -> Text -> [Object] -> Text
attrF m k xs =
  h2 k <>
  code (take m $ (\(k',i) -> fK (show k') (fI i)) <$> countValues k xs)

attrSF :: Text -> [Object] -> Text
attrSF k xs =
  h2 k <>
  code
  ((\(k',i) -> fK (show k') (fI i)) <$>
    sortBy (comparing fst)
    ((\(v,x) -> (unstring v,x)) <$>
      countValues k xs))

attrNF :: Text -> [Object] -> Text
attrNF k xs = h2 k <>
  code ((\(k',i) -> fK (show k') (fI i)) <$>
    sortBy (comparing fst)
     ((\(v,x) -> (unnumber v,x)) <$>
      countValues k xs))

fCard :: Maybe Text -> Map.HashMap Text Value -> Text
fCard h c =
  maybe mempty h2 h <>
  code ((\(k,v) -> fKR k (fV 3 8 v)) <$> Map.toList c)

setEnv :: [Text] -> IO (Either Text Env)
setEnv cardsets = do
  c <- create
  jsons' <- getJsonCards
  let orSet = foldl' (\x a y -> x y || (y==String a)) (const False) cardsets
  let js = filt [("set", orSet)] jsons'
  let colls = filt [("collectible",(==Bool True))] jsons'
  let hps = json2Cards $ filt [ ("type", (== String "HERO_POWER"))] js
  let hps' = Map.fromList <$> fmap (\x -> (cardClass x, x)) <$> hps
  let hs = json2Cards $ filt [ ("type", (== String "HERO"))] js
  let hs' = Map.fromList <$> fmap (\x -> (cardClass x, x)) <$> hs
  let coins = filt [("name", (== String "The Coin"))] jsons'
  let coin = maybe (Left "no coin?") Right (head coins)
  pure $
    Env <$>
    pure c <*>
    json2CardMap js <*>
    json2CardMap colls <*>
    hs' <*>
    hps' <*>
    (join $ json2Card <$> coin)

-- take a cardClass and make a deck from basic and standard
json2CardMap :: [Object] -> Either Text CardMap
json2CardMap os = CardMap <$> Map.fromList <$> fmap (\x -> (sid x,x)) <$>
  json2Cards os

json2Cards :: [Object] -> Either Text [SCard]
json2Cards os
  | ls == [] = Right rs
  | otherwise = Left (Text.intercalate "/n" ls)
  where
    (ls, rs) = partitionEithers (json2Card <$> os)

json2Card :: Object -> Either Text SCard
json2Card o =
  SCard <$> pure c <*> pure a <*> pure h <*> i <*> n <*> ct <*> cc <*> pure r <*> pure sp <*> pure ov <*> pure dur <*> pure target <*> pure ent <*> pure txt
  where
    c = either (const 0) identity (oint o "cost")
    a = either (const 0) identity (oint o "attack")
    h = either (const 0) identity (oint o "health")
    i = Id <$> otext o "id"
    n = otext o "name"
    -- ms = text2mech <$> omech o
    ct = view (from ctypeText) <$> (otext o "type")
    cc = view (from cclassText) <$> (otext o "cardClass")
    r = mtext o "race"
    sp = mint o "spellDamage"
    ov = mint o "overload"
    dur = either (const 0) identity (oint o "durability")
    target = mtext o "targetingArrowText"
    ent = fmap Id <$> otexts o "entourage"
    txt = either (const "") identity (otext  o "text")

-- exploring text elements of json cardset
findText ::
  (Eq k, Hashable k) =>
  k -> (Text -> Bool) -> [Map.HashMap k Value] -> [Map.HashMap k Value]
findText k pr =
  filter
  ( (== Just True) .
    fmap stringp .
    Map.lookup k)
  where
    stringp (String x) = pr x
    stringp _ = False

