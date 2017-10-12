{-# LANGUAGE StandaloneDeriving, GADTs,
    DeriveDataTypeable, FlexibleInstances,
    MultiParamTypeClasses, OverloadedStrings,
    TypeFamilies,  GeneralizedNewtypeDeriving
#-}
module Main where

import Haxl.Core
import Data.Hashable
import Data.Typeable
import Control.Monad

type PostIds = Int
type Contents = String
data BlogRequest a where
  GetPostIds :: BlogRequest [PostIds]
  GetContents :: PostIds -> BlogRequest Contents

deriving instance Show (BlogRequest a)
instance ShowP BlogRequest where showp = show
deriving instance Typeable BlogRequest
instance Hashable (BlogRequest a) where
  hashWithSalt salt GetPostIds = hashWithSalt salt (0::Int)
  hashWithSalt salt (GetContents p) = hashWithSalt salt (1::Int, p)
deriving instance Eq (BlogRequest a)

instance StateKey BlogRequest where
  data State BlogRequest = BlogDataState

instance DataSourceName BlogRequest where
  dataSourceName _ = "BlogDataSource"

instance DataSource u BlogRequest where
  fetch _ _flags _userEnv blockedFetches = SyncFetch $ batchFetch blockedFetches

newtype BlogDBException = BlogDBException String
  deriving (Show, Typeable)

batchFetch :: [BlockedFetch BlogRequest] -> IO ()
batchFetch xs = do
  forM_ xs $ \i ->
    case i of
      BlockedFetch GetPostIds v -> putSuccess v [1,2] 
  return ()

getPostIds :: GenHaxl u [PostIds]
getPostIds = dataFetch GetPostIds

main :: IO ()
main = do
  u <- emptyEnv ()
  runHaxl u getPostIds
  print ""
