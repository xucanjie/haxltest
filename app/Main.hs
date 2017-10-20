module Main where
import           Data.Pool
import           Database.MySQL.Simple
import           Database.MySQL.Simple.Types
import           Haxl.Core
import           Haxl.Prelude
import           Lib
import qualified Prelude                     hiding (mapM)

haxltest xs = do
  indexinfo <- mapM hgetIndexinfo xs
  userinfo <- mapM hgetUserinfo indexinfo
  return (zip3 xs indexinfo userinfo)

main :: IO ()
main = do
  let conn = defaultConnectInfo
  pool <- createPool (connect conn) close 10 1000 10
  let db = (pool, pool)
  env <- initEnv (stateSet (UdbDataState db) stateEmpty) ()
  x <- runHaxl env (haxltest (take 10 [1,2..]))
  print x
