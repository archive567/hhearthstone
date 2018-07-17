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
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
\end{code}

[libraries](https://www.stackage.org/)
---

- [protolude](https://www.stackage.org/package/protolude)

- [readert design](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern)

\begin{code}
import Protolude
import Data.Aeson
import qualified Data.HashMap.Strict as Map
import Data.Scientific
import Hearth
import Hearth.Json
import Hearth.Format
import System.Random.MWC
import Control.Monad.Primitive
import Data.List ((!!))
\end{code}

JSON of cards
--- 

[HearthstoneJSON](https://hearthstonejson.com/docs/cards.html)

[cards](https://api.hearthstonejson.com/v1/latest/enUS/cards.json)

- [x] create a random deck, based on chosen hero
- [ ] count deck, hand, board
- [ ] start game
- [ ] list possible plays
- [ ] make a play

\begin{code}

allClassColls :: (MonadReader Env m) => CardClass -> m [Card]
allClassColls t = do
  env <- ask
  let xs = filter (\x ->
                     (cardClass x == t) ||
                     (cardClass x == Neutral))
           (collectibles env)
  pure $ xs <> xs

-- put the Gen in a State
shuffle :: Gen (PrimState IO) -> Int -> [a] -> IO [a]
shuffle mwc n xs = go n xs []
  where
    go _ [] r = pure $ reverse r
    go n' xs' r
      | n' == 0 = pure $ reverse r
      | otherwise = do
          rv <- uniformR (0,length xs' - 1) mwc
          let (fir, x:sec) = splitAt rv xs'
          go (n' - 1) (fir <> sec) (x:r)

testInitialGameState :: (MonadReader Env m, MonadIO m) => m (Either Text GameState)
testInitialGameState = do
  env <- ask
  mageCards <- allClassColls Mage
  druidCards <- allClassColls Druid
  cc1 <- liftIO $ shuffle (gen env) 30 mageCards
  cc2 <- liftIO $ shuffle (gen env) 30 druidCards
  initGame (Mage, cc1) (Druid, cc2)

shuffleTest :: (MonadReader Env m, MonadIO m) => m [Id]
shuffleTest = do
  env <- ask
  let xs = mage $ collectibles env
  i <- liftIO $ shuffle (gen env) 3 (take (length xs) [0..])
  return $ ((id <$> xs) !!) <$> i
  where
    mage = filter (\x -> cardClass x == Mage)

jsonStats :: [Object] -> Text
jsonStats xs =
    h2 "card set stats" <>
    code ((\(t,s) -> fK t (fS 3 s)) <$>
      [ ("number of cards", int2Sci (length xs))
      , ("attributes per card", fromFloatDigits av)
      ]) <>
    h2 "attribute count" <>
    code ((\(k,i) -> fK k (fI i)) <$>
           sortBy (comparing (Down . snd))
             (Map.toList $ countKeys xs)) <>
    mconcat (zipWith fCard [Just "first card", Just "second card"]
             (take 2 xs)) <>
    h2 "cards with no cardClass" <>
    mconcat (zipWith fCard (Just . show <$> [0..]) $
     filter (not . Map.member "cardClass") xs) <>
    mconcat ((`attrSF` xs) <$>
     [ "set"
     , "type"
     , "cardClass"
     , "rarity"
     ]) <>
    mconcat ((`attrNF` xs) <$>
     [ "cost"
     , "health"
     , "attack"
     ]) <>

    mconcat ((\x -> attrF 20 x xs) <$>
     [ "text"
     , "mechanics"
     , "playRequirements"
     , "collectible"
     , "race"
     , "elite"
     , "referencedTags"
     , "entourage"
     , "targetingArrowText"
     , "durability"
     , "faction"
     , "collectionText"
     , "overload"
     ])
    where
      l = length xs
      av = fromIntegral (sum (Map.size <$> xs)) / fromIntegral l

main :: IO ()
main = do
  env' <- setEnv ["CORE", "EXPERT1"]
  case env' of
    Left x -> print ("environment problem: " <> x)
    Right env -> do
      shuffleIds <- runReaderT shuffleTest env
      xs <- getJsonCards
      writeFile "other/json.md" $
        h2 "shuffle" <> mconcat (art <$> shuffleIds) <>
        jsonStats xs
      game <- runReaderT testInitialGameState env
      case game of
        Left err -> print $ "game state foobarred: " <> err
        Right game' ->
          writeFile "other/testGameState.md" $
          h2 "test game render" <>
          renderGame game'

\end{code}
