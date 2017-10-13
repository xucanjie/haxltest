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

type PostId = Int
type Contents = String
data BlogRequest a where
  GetPostIds :: BlogRequest [PostId]

deriving instance Show (BlogRequest a)
instance ShowP BlogRequest where showp = show
deriving instance Typeable BlogRequest
instance Hashable (BlogRequest a) where
  hashWithSalt salt GetPostIds = hashWithSalt salt ()
deriving instance Eq (BlogRequest a)

instance StateKey BlogRequest where
  data State BlogRequest = BlogDataState

instance DataSourceName BlogRequest where
  dataSourceName _ = "BlogDataSource"

instance DataSource u BlogRequest where
  fetch _ _ _ reqs = SyncFetch $ batchFetch reqs


runX :: BlogRequest a -> ResultVar a -> IO ()
runX GetPostIds var = putSuccess var [1,2]

newtype BlogDBException = BlogDBException String
  deriving (Show, Typeable)

batchFetch :: [BlockedFetch BlogRequest] -> IO ()
batchFetch reqs = forM_ reqs $ \(BlockedFetch req v) -> runX req v

initialState :: StateStore
initialState = stateSet BlogDataState stateEmpty

getPostIds :: GenHaxl u [PostId]
getPostIds = dataFetch GetPostIds

main :: IO ()
main = do
  u <- initEnv initialState ()
  r <- runHaxl u getPostIds
  print r
