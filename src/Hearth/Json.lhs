\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Hearth.Json where

import Data.Aeson
import Data.Scientific
import Hearth.Format
import Protolude
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V
import Hearth
import qualified Data.Text as Text
import System.Random.MWC

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
fCard h card =
  maybe mempty h2 h <>
  code ((\(k,v) -> fKR k (fV 3 8 v)) <$> Map.toList card)

setEnv :: [Text] -> IO (Either Text Env)
setEnv sets = do
  c <- create
  jsons' <- getJsonCards
  let orSet = foldl' (\x a y -> x y || (y==String a)) (const False) sets
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
    json2Cards js <*>
    json2Cards colls <*>
    hs' <*>
    hps' <*>
    join (json2Card <$> coin)

-- take a cardClass and make a deck from basic and standard
json2Cards :: [Object] -> Either Text [Card]
json2Cards os
  | ls == [] = Right rs
  | otherwise = Left (Text.intercalate "/n" ls)
  where
    (ls, rs) = partitionEithers (json2Card <$> os)

json2Card :: Object -> Either Text Card
json2Card o =
  Card <$> pure c <*> pure a <*> pure h <*> i <*> n <*>
  pure ms <*> ct <*> pure [] <*> cc <*> pure r <*> pure sp <*>
  pure ov <*> pure dur <*>
  pure target <*> pure ent <*> pure txt
  where
    c = either (const 0) identity (oint o "cost")
    a = either (const 0) identity (oint o "attack")
    h = either (const 0) identity (oint o "health")
    i = otext o "id"
    n = otext o "name"
    ms = text2mech <$> omech o
    ct = join $ text2CardType <$> otext o "type"
    cc = either Left text2CardClass (otext o "cardClass")
    r = mtext o "race"
    sp = mint o "spellDamage"
    ov = mint o "overload"
    dur = mint o "durability"
    target = mtext o "targetingArrowText"
    ent = otexts o "entourage"
    txt = either (const "") identity (otext  o "text")

\end{code}
