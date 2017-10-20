{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Lib where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception           hiding (try)
import           Data.Hashable
import           Data.List
import qualified Data.Map                    as Map
import           Data.Pool
import           Data.Typeable
import           Database.MySQL.Simple
import           Database.MySQL.Simple.Types
import           Haxl.Core
import           Haxl.Prelude
import qualified Prelude                     hiding (mapM)
import           System.IO
import           Control.Applicative

type Uid = Int
type Shardid = Int
type Idcard = Maybe String
type Realname = Maybe String
type Username = String
type Phone = Maybe String
type IndexInfo = (Uid, Username, Phone, Shardid)
type UserInfo = (Uid, Idcard, Realname)
type DB = (Pool Connection, Pool Connection)

data UdbRequest a where
  GetIndexInfo :: Uid -> UdbRequest IndexInfo
  GetUserInfo :: IndexInfo -> UdbRequest UserInfo
deriving instance Show (UdbRequest a)
deriving instance Eq (UdbRequest a)
deriving instance Typeable UdbRequest

instance ShowP UdbRequest where showp = show

instance Hashable (UdbRequest a) where
  hashWithSalt salt (GetIndexInfo x) = hashWithSalt salt (0::Int, x)
  hashWithSalt salt (GetUserInfo x)  = hashWithSalt salt (1::Int, x)

instance StateKey UdbRequest where
  data State UdbRequest = UdbDataState DB

instance DataSourceName UdbRequest where
  dataSourceName _ = "UdbSource"

instance DataSource u UdbRequest where
  fetch (UdbDataState db) _ _ reqs = SyncFetch $ batchFetch db reqs

batchFetch :: DB -> [BlockedFetch UdbRequest] -> IO ()
batchFetch (db1, db2) reqs = do
  let (as, bs) = splitRequest reqs
  getIndexinfo db1 as
  getUserinfo db2 bs

type CO = ([(Uid, ResultVar IndexInfo)], [(IndexInfo, ResultVar UserInfo)])
splitRequest :: [BlockedFetch UdbRequest] -> CO
splitRequest [] = ([],[])
splitRequest (x:xs) = case x of
                        BlockedFetch (GetIndexInfo uid) var -> ((uid, var):fst pre, snd pre)
                        BlockedFetch (GetUserInfo i) var -> (fst pre, (i,var):snd pre)
  where
    pre = splitRequest xs

class ToMap a where
  toMap :: [a] -> Map.Map Uid a

instance ToMap IndexInfo where
  toMap = foldr (\x@(k,_,_,_) acc -> Map.insert k x acc) Map.empty

instance ToMap UserInfo where
  toMap = foldr (\x@(k,_,_) acc -> Map.insert k x acc) Map.empty

getIndexinfo :: Pool Connection -> [(Uid, ResultVar IndexInfo)] -> IO ()
getIndexinfo db [] = return ()
getIndexinfo db xs = do
  print "BatchFecht: " >> print (length uids)
  res::(Map.Map Uid IndexInfo) <- fmap toMap $ withResource db $ \s -> query s (fromString q) $ Only (In uids)
  forM_ xs $ \(i, v) -> do
    let r = Map.lookup i res
    case r of
      Nothing -> putSuccess v (0, [], Nothing, 0)
      Just c  -> putSuccess v c
  where
    uids = map fst xs :: [Int]
    q = "select uid,username,bindedphone,shardid from ui_index where uid in ?"


hgetIndexinfo :: Uid -> GenHaxl u IndexInfo
hgetIndexinfo = dataFetch . GetIndexInfo

hgetUserinfo :: IndexInfo -> GenHaxl u UserInfo
hgetUserinfo = dataFetch . GetUserInfo

groupQuery :: [IndexInfo] -> Map.Map Shardid [Uid]
groupQuery [] = Map.empty
groupQuery (x@(u,_,_,s):xs) = case ispre of
                                Nothing -> Map.insert s [u] pre
                                Just uids -> Map.update (\a -> Just (u:a)) s pre
  where
    pre = groupQuery xs
    ispre = Map.lookup s pre

getUserinfo :: Pool Connection -> [(IndexInfo, ResultVar UserInfo)] -> IO ()
getUserinfo db [] = return ()
getUserinfo db xs = do
  res <-  (toMap . concat) Control.Applicative.<$>  mapConcurrently doGetone r
  forM_ xs $ \((u,_,_,_),v) -> case Map.lookup u res of
                                 Nothing -> putSuccess v (0, Nothing, Nothing)
                                 Just r -> putSuccess v r
  where
    r = Map.toList . groupQuery $ filter (\(u,_,_,_) -> u /= 0 ) $ map fst xs
    doGetone :: (Shardid, [Uid]) -> IO [UserInfo]
    doGetone (x, uids) =
      withResource db $ \p -> query p (fromString q) $ Only (In uids)
      where
        q =  "select uid,idcard, realname from user_info_" ++ table ++ " where uid in ?"
        table
          | x < 10 = "00" ++ show x
          | otherwise = "0" ++ show x
