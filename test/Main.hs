{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main )
where

import State.Types ( State, ChapterId, mkChapterId, CommentId
                   , Comment, getCounts, findComments, mkCommentId
                   , addChapter, Comment(..), addComment, SessionId(..)
                   , getLastInfo, SessionInfo(..)
                   )
import qualified State.Mem
import qualified State.Disk
import qualified State.SQLite

import Data.List ( sort )
import Data.Maybe ( fromJust )
import Data.Array.Unboxed ( UArray, listArray, bounds, (!) )
import Data.Char ( isPrint, chr )
import System.Directory ( createDirectory, getTemporaryDirectory, removeDirectoryRecursive )
import System.FilePath ( (</>) )
import Control.Monad ( replicateM, forM_, unless, join, replicateM_, foldM )
import Control.Monad.Random ( getRandomR, getRandom )
import Control.Applicative ( (<$>), (<*>) )
import Control.Exception ( finally )
import System.IO ( stdout, hFlush )
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as E

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
                      SQLite -> State.SQLite.new (d </> "state.db")
  withTemporaryDirectory $ \d ->
      do [st1, st2] <- zip ts <$> mapM (mkStore d) ts
         act st1 st2

data Operation = GetCounts (Maybe ChapterId)
               | FindComments CommentId
               | AddChapter ChapterId [CommentId]
               | AddComment CommentId (Maybe ChapterId) Comment
               | GetLastInfo SessionId
                 deriving Show

data Result = Unit
            | LastInfo (Maybe SessionInfo)
            | Counts [(CommentId, Int)]
            | Comments [Comment] deriving (Eq, Show)

applyOp :: Operation -> State -> IO Result
applyOp (GetCounts p) st            = Counts . sort <$> getCounts st p
applyOp (FindComments cId) st       = Comments <$> findComments st cId
applyOp (AddChapter chId cIds) st   = addChapter st chId cIds >> return Unit
applyOp (AddComment cId mChId c) st = addComment st cId mChId c >> return Unit
applyOp (GetLastInfo sId) st        = LastInfo <$> getLastInfo st sId

choice :: [a] -> IO a
choice xs = (xs !!) <$> getRandomR (0, length xs - 1)

randomOp :: IO Operation
randomOp = join $ choice $
           [ GetCounts <$> maybeRand randomChapterId
           , FindComments <$> randomCommentId
           , let cIds = do
                   n <- getRandomR (0, 100)
                   replicateM n $ randomCommentId
             in AddChapter <$> randomChapterId <*> cIds
           , AddComment
             <$> randomCommentId
             <*> maybeRand randomChapterId
             <*> randomComment
           , GetLastInfo <$> randomSessionId
           ]

maybeRand :: IO a -> IO (Maybe a)
maybeRand x = do
  doGen <- getRandom
  if doGen then Just <$> x else return Nothing

randomChapterId :: IO ChapterId
randomChapterId = fromJust . mkChapterId <$> randomText

randomCommentId :: IO CommentId
randomCommentId = fromJust . mkCommentId <$> randomText

randomSessionId :: IO SessionId
randomSessionId = SessionId <$> randomBS

usableChars :: UArray Int Char
usableChars = listArray (0, length cs - 1) cs
    where
      cs = filter isPrint [chr 0..chr 127]

randomBS :: IO B.ByteString
randomBS = E.encodeUtf8 <$> randomText

randomText :: IO T.Text
randomText = join $ choice [commonValue, completelyRandom]
    where
      completelyRandom = do
        let randomChar = do
              i <- getRandomR $ bounds usableChars
              return $ usableChars ! i
        n <- getRandomR (2, 40)
        T.pack <$> replicateM n randomChar
      commonValue = choice ["alpha", "beta", "gamma", "delta"]

doRandomOperations :: (StoreType, State) -> (StoreType, State) -> IO ()
doRandomOperations (ty1, st1) (ty2, st2) = do
  n <- getRandomR (10, 500)
  ops <- replicateM n randomOp
  _ <- foldM (\soFar op -> do
                r1 <- applyOp op st1
                r2 <- applyOp op st2
                unless (r1 == r2) $ do
                  putStrLn $ "Got different results for " ++ show op ++ " on " ++
                           show ty1 ++ " and " ++ show ty2
                  forM_ (soFar [op]) print
                  putStrLn $ "Results were:"
                  print ty1
                  print r1
                  putStrLn $ replicate 50 '-'
                  print ty2
                  print r2
                  putStrLn $ replicate 50 '-'
                  error "Test failed"
                return (soFar . (op:))) id ops
  return ()

randomComment :: IO Comment
randomComment = Comment <$> randomText <*> randomText <*> maybeRand randomText <*> randomTime <*> randomSessionId
    where
      randomTime = realToFrac <$> (getRandomR (0, 2**32) :: IO Double)

main :: IO ()
main = do
  putStrLn "Running randomized store tests"
  replicateM_ 100 $ do
         withRandomStores doRandomOperations
         putStr "."
         hFlush stdout
  putStrLn "\ndone. Tests passed."

