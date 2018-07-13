[hheartstone](https://github.com/tonyday567/hheartstone)
===

Modelling Heartstone

Gabrielle on [game simulation](http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html)

Game engine library (console not working in macos): [heartshroud](https://github.com/thomaseding/hearthshroud)

Get decks from hearthpwm: [blender](https://github.com/blender/Hearthstone)
Card manager: [HCM](https://github.com/nicuveo/HCM)
Simulator in python: [fireplace](https://github.com/jleclanche/fireplace)


[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
---

\begin{code}
{-# OPTIONS_GHC -Wall #-}
\end{code}

[pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
---

\begin{code}
-- doctest doesn't look at the cabal file, so you need pragmas here
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
\end{code}

[libraries](https://www.stackage.org/)
---

- [protolude](https://www.stackage.org/package/protolude)

\begin{code}
import Protolude hiding ((%))
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V
import Formatting
import Data.Scientific
\end{code}

code
===

- [hoogle](https://www.stackage.org/package/hoogle)

ADT
---

\begin{code}

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

JSON of cards
---

[HearthstoneJSON](https://hearthstonejson.com/docs/cards.html)

[cards](https://api.hearthstonejson.com/v1/latest/enUS/cards.json)

\begin{code}

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

getJsonCards :: IO [Object]
getJsonCards = do
  t <- B.readFile "other/cards.json"
  pure $ maybe [] (fmap unobject . unarray) (decode t)

countKeys :: [Object] -> Map.HashMap Text Integer
countKeys = foldl' u Map.empty where
  u x a = Map.unionWith (+) x (Map.map (const 1) a)

countAllValues :: [Object] -> Map.HashMap (Text, Value) Integer
countAllValues = foldl' u Map.empty where
  u x a = Map.unionWith (+) x (Map.fromList $ (\(k,v) -> ((k,v),1)) <$> Map.toList a)

countValues :: Text -> [Object] -> [(Value, Integer)]
countValues k' xs = ((\((_ , v),x) -> (v, x))) <$> (Map.toList $ Map.filterWithKey (\(k,_) _ -> k ==k') (countAllValues xs))

attrF :: Int -> Text -> [Object] -> Text
attrF m k xs = h2 k <> (code $ take m $ (\(k',i) -> fK (show k') (fI i)) <$>
                      (countValues k xs))

attrSF :: Text -> [Object] -> Text
attrSF k xs = h2 k <> (code $ (\(k',i) -> fK (show k') (fI i)) <$>
                      (sortBy (comparing fst)) ((\(v,x) -> (unstring v,x)) <$>
                                                countValues k xs))

attrNF :: Text -> [Object] -> Text
attrNF k xs = h2 k <>
  (code $ (\(k',i) -> fK (show k') (fI i)) <$>
    sortBy (comparing fst)
     ((\(v,x) -> (unnumber v,x)) <$>
      countValues k xs))

fK :: Text -> Text -> Text
fK t1 t2 = sformat ((right 20 ' ' %. stext) % (left 10 ' ' %. stext)) t1 t2

fKR :: Text -> Text -> Text
fKR t1 t2 = sformat ((right 20 ' ' %. stext) % (right 20 ' ' %. stext)) t1 t2

fI :: (Integral a, Buildable a) => a -> Text
fI x = sformat commas x

fV :: Int -> Int -> Value -> Text
fV i _ (String x) = sformat (left i ' ' %. stext) x
fV i p (Number x) = sformat (right i ' ' %. stext) (fS p x)
fV i _ x = sformat (right i ' ' %. stext) (show x)

fS :: Int -> Scientific -> Text
fS p x = case (floatingOrInteger x) of
  Left _ -> sformat (prec p) x
  Right (i :: Int) -> sformat commas i

fCard :: Maybe Text -> Map.HashMap Text Value -> Text
fCard h card =
  maybe mempty h2 h <>
  (code $ ((\(k,v) -> fKR k (fV 3 8 v))) <$> (Map.toList card))

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

main :: IO ()
main = do
  -- o :: Opts Unwrapped <- unwrapRecord "hheartstone"
  -- let n = fromMaybe 10 (number o)
  xs <- getJsonCards
  let l = length xs
  let av = fromIntegral (sum (Map.size <$> xs)) / fromIntegral l
  writeFile "other/json.md" $
    h2 "card set stats" <>
    code ((\(t,s) -> fK t (fS 3 s)) <$>
      [ ("number of cards", int2Sci (length xs))
      , ("attributes per card", fromFloatDigits av)
      ]) <>
    h2 "attribute count" <>
    (code ((\(k,i) -> fK k (fI i)) <$>
           (sortBy (comparing (Down . snd)) $
             Map.toList $ countKeys xs))) <>
    (mconcat $ zipWith fCard [Just "first card", Just "second card"] (take 2 xs)) <>
    h2 "cards with no cardClass" <>
    (mconcat $ zipWith fCard (Just . show <$> [0..]) $
     filter (not . (Map.member "cardClass")) xs) <>
    (mconcat $ (\x -> attrSF x xs) <$>
     [ "set"
     , "type"
     , "cardClass"
     , "rarity"
     ]) <>
    (mconcat $ (\x -> attrNF x xs) <$>
     [ "cost"
     , "health"
     , "attack"
     ]) <>

    (mconcat $ (\x -> attrF 20 x xs) <$>
     [ "dbfId"
     , "text"
     , "artist"
     , "mechanics"
     , "playRequirements"
     , "collectible"
     , "flavor"
     , "race"
     , "elite"
     , "referencedTags"
     , "entourage"
     , "howToEarnGolden"
     , "howToEarn"
     , "targetingArrowText"
     , "durability"
     , "hideStats"
     , "faction"
     , "collectionText"
     , "overload"
     , "spellDamage"
     , "armor"
     , "multiClassGroup"
     , "classes"
     , "questReward"
     ])

\end{code}

output
---

```include
other/json.md
```

workflow
===

```
stack build --exec "$(stack path --local-install-root)/bin/hheartstone" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i app/example.lhs -t markdown -o readme.md --filter pandoc-include --mathjax"
```
