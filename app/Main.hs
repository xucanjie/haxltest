{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ApplicativeDo              #-}
module Main where

--import           Control.Monad
import qualified Prelude hiding(mapM)
import   Haxl.Prelude
import           Data.Hashable
import           Data.Typeable
import           Haxl.Core
import           System.IO
--import Control.Concurrent.STM
import Control.Concurrent
type PostId = Int
type Contents = String
data BlogRequest a where
  GetPostIds :: BlogRequest [PostId]
  GetContents :: PostId -> BlogRequest Contents
deriving instance Show (BlogRequest a)
instance ShowP BlogRequest where showp = show
deriving instance Typeable BlogRequest
instance Hashable (BlogRequest a) where
  hashWithSalt salt GetPostIds      = hashWithSalt salt (0::Int)
  hashWithSalt salt (GetContents x) = hashWithSalt salt (1::Int, x)
deriving instance Eq (BlogRequest a)

instance StateKey BlogRequest where
  data State BlogRequest = BlogDataState

instance DataSourceName BlogRequest where
  dataSourceName _ = "BlogDataSource"

instance DataSource u BlogRequest where
  fetch _ _ _ reqs = SyncFetch $ batchFetch reqs

runX :: BlogRequest a -> ResultVar a -> IO ()
runX GetPostIds var      = putSuccess var [1,2,3,10,12,3,1,23,4,5,6,7,8,9,0]
runX (GetContents x) var = putSuccess var (show x)

batchContent :: [BlockedFetch BlogRequest] -> ([ResultVar [PostId]],[(PostId,ResultVar Contents)])
batchContent [] = ([],[])
batchContent (x:xs) = case x of
                          BlockedFetch (GetContents v)  var -> (fst pre, (v, var):snd pre)
                          BlockedFetch GetPostIds var -> (var:fst pre, snd pre)
  where pre = batchContent xs

runPostids :: [ResultVar [PostId]] -> IO ()
runPostids xs = forM_ xs $ \i -> putSuccess i [1,2,3,4,5,6,7,8,9,10,11,12,13]

runGetContents :: [(PostId,ResultVar Contents)] -> IO ()
runGetContents [] = return ()
runGetContents xs = do
  print "batchFetch..."
  print ids --we can batch fetch data here, or we can concrrent fetch data here
  forM_ (zip ids (map snd xs) ) $ \(i, v) -> putSuccess v i
  where ids = map (show . fst) xs

batchFetch :: [BlockedFetch BlogRequest] -> IO ()
batchFetch reqs = do
  let (bs, as) = batchContent reqs
  runPostids bs
  runGetContents as


initialState :: StateStore
initialState = stateSet BlogDataState stateEmpty

getPostIds :: GenHaxl u [PostId]
getPostIds = dataFetch GetPostIds

getContent :: PostId -> GenHaxl u Contents
--getContent = uncachedRequest . GetContents
getContent = dataFetch . GetContents


haxltest = do
  postids <- getPostIds
  a <- mapM getContent postids
  b <- getContent 10
  c <- getContent 11
  return (a, b, c)

main :: IO ()
main = do
  u <- initEnv initialState ()
  --r <- runHaxl u getPostIds
  print "fetch 1"
  b <- runHaxl u haxltest
  print "fetch 2 cached"
  r <- runHaxl u haxltest
  print "resut:"
  print (b, r)
