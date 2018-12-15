{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Hearth.Xml where

import Control.Lens hiding (Magma)
import Control.Monad.Primitive
import Data.Generics.Product (field)
import GHC.Show
import Hearth.Enum
import Hearth.Card
import Hearth.Player
import NumHask.Prelude hiding (First, from, to)
import System.Random.MWC
import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Xeno.DOM
import Control.Exception.Safe
import Data.Tree


-- | processing a tree
-- >>> t <- toTree <$> read "other/hsreplay/example.hsreplay"
-- >>> branchCount t
-- (96,525)
--
-- >>> (ts,s) <- step 5 ([t],newStateXml)
-- s
-- StateXml {entities = fromList [(1,fromList [(49,1),(20,1),(53,1),(198,4),(202,1),(204,2)]),(2,fromList [(272,1),(176,10),(17,1),(49,1),(50,1),(53,2),(202,2),(27,64),(28,10),(29,4),(30,1),(31,1),(399,4)]),(3,fromList [(272,1),(176,10),(17,1),(49,1),(50,2),(53,3),(23,1),(24,1),(202,2),(27,66),(28,10),(29,4),(30,2),(31,2),(399,3)])], players = fromList [(2,fromList [("accountLo","27390670"),("accountHi","144115193835963207"),("id","2"),("playerID","1"),("name","Adys")]),(3,fromList [("accountLo","0"),("accountHi","144115188075855872"),("id","3"),("playerID","2"),("name","The Innkeeper")])], globalState = GlobalState {globalTurn = 0, globalLog = "", globalTimeStamp = "18:57:36.505394", leaves = 5}}
--
-- >>> (ts,s) <- step 246 ([t],newStateXml)
-- FIXME: up to Options Element

read :: FilePath -> IO Node
read f = do
  bs <- B.readFile f
  either throw pure (parse bs)

data El = El { elName :: ByteString, elKv :: [(ByteString,ByteString)]} deriving (Eq, Show)

toTree :: Node -> Tree El
toTree = unfoldTree (\b -> (El (Xeno.DOM.name b) (attributes b), Xeno.DOM.children b))

allNames t = sortBy (compare)
  (Map.toList $
   foldl' (\x a ->
             Map.insertWith
            (const (+1))
            (elName a) 1 x)
   Map.empty t)

nks t = (Map.toList $ foldl' (\x a -> Map.insertWith (const (+1)) ([elName a] <> (fst <$> elKv a)) 1 x) Map.empty t)

nkvs t = (Map.toList $ foldl' (\x a -> Map.insertWith (const (+1)) ([elName a] <> k' a) 1 x) Map.empty t) where
  k' = (\a -> mconcat $ (\(x,y) -> [x,y]) <$> (elKv a)) :: El -> [ByteString]

readI :: Int -> ByteString -> Int
readI def bs = maybe def fst (C.readInt bs)

kvs :: (Hashable a, Hashable b, Eq a, Eq b) => [(a,b)] -> Map.HashMap a b
kvs as = foldl' (\x (k,v) -> Map.insert k v x) Map.empty as

kvs' :: Int -> [(ByteString,ByteString)] -> Map.HashMap Int Int
kvs' def bs = foldl' (\x (k,v) -> Map.insert (readI def k) (readI def v) x) Map.empty bs

findv :: (Hashable a, Hashable b, Eq a, Eq b) => [(a,b)] -> a -> Maybe b
findv as a = Map.lookup a (kvs as)

findi :: [(ByteString,ByteString)] -> ByteString -> Maybe Int
findi as a = join $ fmap fst . C.readInt <$> Map.lookup a (kvs as)

findId :: [(ByteString,ByteString)] -> Maybe Int
findId as = findi as "id"

branchCount :: Tree a -> (Int, Int)
branchCount t = (length (flatten i), sum (flatten i)) where
  i = count1 t

count1 :: Tree a -> Tree Int
count1 t = maybe (Node 0 []) identity (go t) where
  go (Node _ []) = Nothing
  go (Node _ ts) = Just $ Node (length ts) (catMaybes $ go <$> ts)

data StateXml = StateXml
  { entities :: Map.HashMap Int (Map.HashMap Int Int)
  , showEntities :: Map.HashMap Int (Map.HashMap Int Int)
  , hideEntities :: Map.HashMap Int Int
  , players :: Map.HashMap Int (Map.HashMap ByteString ByteString)
  , cards :: Map.HashMap Int (Map.HashMap ByteString ByteString)
  , globalState :: GlobalState
  } deriving (Show, Eq, Generic)

newStateXml = StateXml Map.empty Map.empty Map.empty Map.empty Map.empty newGlobalState

data WhichChoice = NoChoice | InChoice | InChosen | InSendChoices
  deriving (Show, Eq, Generic)

data GlobalState = GlobalState
  { globalTurn :: Int
  , globalLog :: Text
  , globalTimeStamp :: ByteString
  , leaves :: Int
  , inBlock :: Bool
  , blockKvs :: [(ByteString, ByteString)]
  , whichChoice :: WhichChoice
  , choiceKvs :: [(ByteString, ByteString)]
  , choiceEntities :: Map.HashMap Int Int
  , optionsKvs :: [(ByteString, ByteString)]
  , options :: Map.HashMap Int [(ByteString, ByteString)]
  , sendOptionKvs :: [(ByteString, ByteString)]
  } deriving (Show, Eq, Generic)

newGlobalState = GlobalState 0 "" "" 0 False [] NoChoice [] Map.empty [] Map.empty []

inc :: StateXml -> StateXml
inc s = s & field @"globalState" . field @"leaves" %~ (+1)

ts :: Maybe ByteString -> StateXml -> StateXml
ts bs s = case bs of
  Nothing -> s
  Just bs' -> s & field @"globalState" . field @"globalTimeStamp" .~ bs'

etv_ :: [(ByteString,ByteString)] -> Maybe (Map.HashMap Int (Map.HashMap Int Int))
etv_ as = (\e t v -> Map.singleton e (Map.singleton t v)) <$> e <*> t <*> v
  where
  e = findi as "entity"
  t = findi as "tag"
  v = findi as "value"

showetv_ :: Int -> [(ByteString,ByteString)] -> Maybe (Map.HashMap Int (Map.HashMap Int Int))
showetv_ e as = (\e t v -> Map.singleton e (Map.singleton t v)) <$> pure e <*> t <*> v
  where
  t = findi as "tag"
  v = findi as "value"

processTag :: Tree El -> Either Text (Int,Int)
processTag (Node (El "Tag" as) []) = maybe (Left "bad keys") Right ((,) <$> t <*> v)
  where
  t = findi as "tag"
  v = findi as "value"
processTag x = Left "Not a Tag"

processTags :: [Tree El] -> ([Text], (Map.HashMap Int Int))
processTags ts = (ls, rs')
  where
    (ls, rs) = partitionEithers $ processTag <$> ts
    rs' = foldl' (\x (k,v) -> Map.insert k v x) Map.empty rs

x1 :: (Tree El, StateXml) -> Either Text ([Tree El], StateXml)
x1 (Node (El "HSReplay" _) xs, _) = Right (xs, inc newStateXml)
x1 (Node (El "Game" as) xs, s) = Right (xs, inc (ts as s)) where
  ts as = case findv as "ts" of
    Nothing -> identity
    Just ts' -> field @"globalState" . field @"globalTimeStamp" .~ ts'
x1 (Node (El "GameEntity" as) tags, s) =
  case findId as of
    Nothing -> Left "crap"
    Just i -> Right
      ([], s & inc & field @"entities" %~ Map.insert i (snd $ processTags tags))
x1 (Node (El "Player" as) tags, s) =
  case findId as of
    Nothing -> Left "crap"
    Just i -> Right
      ([],
       s &
       inc &
       field @"entities" %~ Map.insert i (snd $ processTags tags) &
       field @"players" %~ Map.insert i (kvs as))

x1 (Node (El "FullEntity" as) tags, s) =
  case findId as of
    Nothing -> Left "crap"
    Just i -> Right
      ([],
       s &
       inc &
       field @"entities" %~ Map.insert i (snd $ processTags tags) &
       field @"cards" %~ Map.insert i (kvs as))

x1 (Node (El "ShowEntity" as) tags, s) =
  case findi as "entity" of
    Nothing -> Left "crap"
    Just e -> Right
      ([],
       s &
       inc &
       field @"showEntities" %~ Map.union (foldr Map.union Map.empty (x1Tags e tags)))

x1 (Node (El "HideEntity" as) tags, s) =
  case findi as "entity" of
    Nothing -> Left "crap"
    Just e -> case findi as "zone" of
      Nothing -> Left "crap"
      Just z -> Right
        ([],
         s &
         inc &
         field @"hideEntities" %~ Map.insert e z)


x1 (Node (El "TagChange" as) [], s) =
  case etv_ as of
    Nothing -> Left "crap etv"
    Just x -> Right ([], s & inc & field @"entities" %~ Map.union x)

x1 (Node (El "Block" as) xs, s) =
  Right (xs <> [Node (El "EndBlock" []) []]
        , s & inc & ts ts' &
          field @"globalState" . field @"inBlock" .~ True &
          field @"globalState" . field @"blockKvs" .~ as)
  where
    ts' = findv as "ts"

x1 (Node (El "EndBlock" []) [], s) =
  Right ([]
        , s &
          field @"globalState" . field @"inBlock" .~ False &
          field @"globalState" . field @"blockKvs" .~ [])

x1 (Node (El "Choices" as) xs, s) =
  Right (xs <> [Node (El "EndChoices" []) []]
        , s & inc & ts ts' &
          field @"globalState" . field @"whichChoice" .~ InChoice &
          field @"globalState" . field @"choiceKvs" .~ as &
          field @"globalState" . field @"choiceEntities" .~ Map.empty
        )
  where
    ts' = findv as "ts"

x1 (Node (El "Choice" as) [], s) =
  Right ([]
        , s & inc & act
        )
  where
    i = findi as "index"
    e = findi as "entity"
    ins = maybe identity identity (Map.insert <$> i <*> e)
    act = field @"globalState" . field @"choiceEntities" %~ ins

x1 (Node (El "EndChoices" []) [], s) =
  Right ([]
        , s &
          field @"globalState" . field @"whichChoice" .~ NoChoice
        )

x1 (Node (El "ChosenEntities" as) xs, s) =
  Right (xs <> [Node (El "EndChosenEntities" []) []]
        , s & inc & ts ts' &
          field @"globalState" . field @"whichChoice" .~ InChosen &
          field @"globalState" . field @"choiceKvs" .~ as &
          field @"globalState" . field @"choiceEntities" .~ Map.empty
        )
  where
    ts' = findv as "ts"

x1 (Node (El "EndChosenEntities" []) [], s) =
  Right ([]
        , s &
          field @"globalState" . field @"whichChoice" .~ NoChoice
        )

x1 (Node (El "SendChoices" as) xs, s) =
  Right (xs <> [Node (El "EndSendChoices" []) []]
        , s & inc & ts ts' &
          field @"globalState" . field @"whichChoice" .~ InSendChoices &
          field @"globalState" . field @"choiceKvs" .~ as &
          field @"globalState" . field @"choiceEntities" .~ Map.empty
        )
  where
    ts' = findv as "ts"

x1 (Node (El "EndSendChoices" []) [], s) =
  Right ([]
        , s &
          field @"globalState" . field @"whichChoice" .~ NoChoice
        )

x1 (Node (El "Options" as) xs, s) =
  Right ([]
        , s & inc & ts ts' &
          field @"globalState" . field @"optionsKvs" .~ as &
          field @"globalState" . field @"options" .~ (foldr Map.union Map.empty (x1Options xs))
        )
  where
    ts' = findv as "ts"

x1 (Node (El "SendOption" as) [], s) =
  Right ([]
        , s & inc & ts ts' &
          field @"globalState" . field @"sendOptionKvs" .~ as
        )
  where
    ts' = findv as "ts"

x1 (Node (El n as) xs, s) = Left $ "Unknown Node pattern" <> NumHask.Prelude.show n

x1Tag e (Node (El "Tag" as) []) =
  case showetv_ e as of
    Nothing -> Left "crap x1Tag"
    Just x -> Right x

x1Tags e ns = rights $ (x1Tag e) <$> ns

x1Option (Node (El "Option" as) []) =
  case findi as "index" of
    Nothing -> Left "crap x1Option"
    Just i -> Right $ Map.singleton i as

x1Options ns = rights $ x1Option <$> ns




xs1 :: ([Tree El],StateXml) -> Either Text ([Tree El], StateXml)
xs1 ([],s) = Right ([], s)
xs1 ((t:ts),s) = (\(ts',s) -> (ts'<>ts,s)) <$> x1 (t,s)

data HearthException
  = HearthParseError Text
  deriving (Show, Typeable, Generic)

instance Exception HearthException where displayException = GHC.Show.show

next :: ([Tree El], StateXml) -> IO ([Tree El], StateXml)
next x = either (throw . HearthParseError) pure (xs1 x)

step :: Int -> ([Tree El], StateXml) -> IO ([Tree El], StateXml)
step n x = go n x where
  go 0 x' = pure x'
  go n' x' = next =<< (go (n' - 1) x')
