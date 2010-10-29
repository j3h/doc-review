module Main
    ( main )
where

import State.Types ( State, ChapterId )
import qualified State.Mem
import qualified State.Disk

import System.Directory ( createDirectory, getTemporaryDirectory, removeDirectoryRecursive )
import System.FilePath ( (</>) )
import Control.Monad ( replicateM )
import Control.Monad.Random ( getRandomR )
import Control.Applicative ( (<$>) )
import Control.Exception ( finally )

withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory act = do
  t <- getTemporaryDirectory
  let choices = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
      rndChar = (choices !!) <$> getRandomR (0, length choices - 1)
  randPart <- replicateM 8 rndChar
  let d = t </> ("test-" ++ randPart)
  createDirectory d
  act d `finally` removeDirectoryRecursive d

data StoreType = Disk | SQLite | Mem deriving (Show, Eq)

withRandomStores :: ((StoreType, State) -> (StoreType, State) -> IO a) -> IO a
withRandomStores act = do
  elim <- getRandomR (0, 2)
  let (before, (_:after)) = splitAt elim [Disk, SQLite, Mem]
      ts@[t1, t2] = before ++ after
      mkStore d t = case t of
                      Mem    -> State.Mem.new
                      Disk   -> State.Disk.new (d </> "flat-file")
                      SQLite -> State.Disk.new (d </> "state.db")
  withTemporaryDirectory $ \d ->
      do [st1, st2] <- zip ts <$> mapM (mkStore d) ts
         act st1 st2

data Operation = GetCounts (Maybe ChapterId) deriving Show

randomOp :: IO Operation
randomOp = return $ GetCounts Nothing

main :: IO ()
main = withRandomStores $ \(ty1, st1) (ty2, st2) -> print (ty1, ty2)