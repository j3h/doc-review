{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main )
where

import State.Types ( State, ChapterId, mkChapterId, CommentId
                   , Comment, getCounts, findComments, mkCommentId
                   , addChapter, Comment(..), addComment, SessionId(..)
                   , getLastInfo, SessionInfo(..), getChapterComments
                   , getChapterURI
                   )
import qualified State.Mem
import qualified State.Disk
import qualified State.SQLite

import Network.URI ( URI, parseRelativeReference )
import Data.Function ( on )
import Data.List ( sort, sortBy, intercalate )
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

data StoreType = Disk | SQLite | Mem deriving (Show, Eq, Enum, Bounded)

mkStore :: FilePath -> StoreType -> IO State
mkStore d t = case t of
                Mem    -> State.Mem.new
                Disk   -> State.Disk.new (d </> "flat-file")
                SQLite -> State.SQLite.new (d </> "state.db")

withRandomStores :: ((StoreType, State) -> (StoreType, State) -> IO a) -> IO a
withRandomStores act = do
  elim <- getRandomR (0, 2)
  let (before, (_:after)) = splitAt elim [Disk, SQLite, Mem]
      ts = before ++ after
  withTemporaryDirectory $ \d ->
      do [st1, st2] <- zip ts <$> mapM (mkStore d) ts
         act st1 st2

data Operation = GetCounts (Maybe ChapterId)
               | FindComments CommentId
               | AddChapter ChapterId [CommentId] (Maybe URI)
               | AddComment CommentId (Maybe ChapterId) Comment
               | GetLastInfo SessionId
               | GetChapterComments ChapterId
               | GetChapterURI ChapterId
                 deriving Show

data Result = Unit
            | LastInfo (Maybe SessionInfo)
            | Counts [(CommentId, Int)]
            | Comments [Comment]
            | ChapterComments [(CommentId, Comment)]
            | ChapterURI (Maybe URI)
              deriving (Eq, Show)

applyOp :: Operation -> State -> IO Result
applyOp (GetCounts p) st               = Counts . sort <$> getCounts st p
applyOp (FindComments cId) st          = Comments <$> findComments st cId
applyOp (AddChapter chId cIds mURI) st = addChapter st chId cIds mURI >> return Unit
applyOp (AddComment cId mChId c) st    = addComment st cId mChId c >> return Unit
applyOp (GetLastInfo sId) st           = LastInfo <$> getLastInfo st sId
applyOp (GetChapterComments chId) st   = ChapterComments <$> getChapterComments st chId
applyOp (GetChapterURI chId) st        = ChapterURI <$> getChapterURI st chId

choice :: [a] -> IO a
choice xs = (xs !!) <$> getRandomR (0, length xs - 1)

shuffle :: [a] -> IO [a]
shuffle xs = do
  is <- replicateM (length xs) getRandom :: IO [Double]
  return $ map snd $ sortBy (compare `on` fst) $ zip is xs

randomOp :: IO Operation
randomOp = join $ choice $ readOps ++ writeOps

randomWriteOp :: IO Operation
randomWriteOp = join $ choice writeOps

readOps :: [IO Operation]
readOps = [ GetCounts <$> maybeRand randomChapterId
          , FindComments <$> randomCommentId
          , GetLastInfo <$> randomSessionId
          , GetChapterComments <$> randomChapterId
          , GetChapterURI <$> randomChapterId
          ]

writeOps :: [IO Operation]
writeOps = [ let cIds = do
                   n <- getRandomR (0, 100)
                   replicateM n $ randomCommentId
             in AddChapter <$>
                randomChapterId <*>
                cIds <*>
                maybeRand randomRelativeReference
           , AddComment
             <$> randomCommentId
             <*> maybeRand randomChapterId
             <*> randomComment
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

randomRelativeReference :: IO URI
randomRelativeReference = do
  nSegs <- getRandomR (0, 5)
  let getSeg = join $ choice
               [ choice ["a", "b", "c"]
               , do segLength <- getRandomR (1, 10)
                    replicateM segLength $
                               choice $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
               ]
  fromJust . parseRelativeReference . ('/':) . intercalate "/" <$> replicateM nSegs getSeg

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

randomOps :: Int -> Int -> IO [Operation]
randomOps minOps maxOps = do
  n <- getRandomR (minOps, maxOps)
  replicateM n randomOp

doRandomOperations :: (StoreType, State) -> (StoreType, State) -> IO ()
doRandomOperations (ty1, st1) (ty2, st2) = do
  ops <- randomOps 10 500
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
randomComment = Comment
                <$> randomText
                <*> randomText
                <*> maybeRand randomText
                <*> randomTime
                <*> randomSessionId
    where
      -- This one's a little sketchy, because we expect that the time
      -- of each comment will be monotonically non-decreasing. The
      -- stores should act sanely in the face of arbitrary time
      -- values, however, so I'll leave it as a completely random
      -- value.
      randomTime = realToFrac <$> (getRandomR (0, 2**32) :: IO Double)

-- |Test that adding a comment to a node makes it show up at the end
-- of the list of comments when it is subsequently loaded
storeLoadComment :: State -> IO ()
storeLoadComment st = do
  cId <- randomCommentId
  cs <- findComments st cId
  c <- randomComment
  addComment st cId Nothing c
  cs' <- findComments st cId
  unless (cs' == (cs ++ [c])) $
         error "Expected the added comment to come up after adding"

-- |Test that a comment for a node that is part of a chapter is
-- returned when we request comments for this chapter
storeLoadCommentChapter :: State -> IO ()
storeLoadCommentChapter st = do
  chId <- randomChapterId
  cId <- randomCommentId
  ref <- maybeRand randomRelativeReference
  addChapter st chId [cId] ref
  cs <- getChapterComments st chId
  c <- randomComment
  addComment st cId (Just chId) c
  let e = (cId, c)
  cs' <- getChapterComments st chId
  unless (length cs' == length cs + 1) $
         error $ unlines [ "Adding a comment to a chapter didn't add:"
                         , show cs
                         , show cs'
                         ]
  unless (e `elem` cs') $
         error "The expected pair is not part of the comments"

chapterCommentsOrderedByDate :: State -> IO ()
chapterCommentsOrderedByDate st = do
  cs <- getChapterComments st =<< randomChapterId
  let dates = map (cDate . snd) cs
      ordered = and $ zipWith (>=) dates (drop 1 dates)
  unless ordered $
         error "The comments are not ordered by date"

-- |Test some property (possibly with a side-effect)
runArbitraryTest :: State -> IO ()
runArbitraryTest st = ($ st) =<< choice allProps

-- |All defined properties
allProps :: [State -> IO ()]
allProps = [ storeLoadComment
           , chapterCommentsOrderedByDate
           , storeLoadCommentChapter
           ]

-- |Run all of the properties at least once, in an arbitrary order
runRandomTests :: State -> IO ()
runRandomTests st = do
  m <- getRandomR (0, 100)
  let randomProps = replicate m $ runArbitraryTest st
  props <- shuffle $ map ($ st) allProps ++ randomProps
  sequence_ props

main :: IO ()
main = do
  putStr "Testing store properties: "
  hFlush stdout

  -- Test properties of stores
  withTemporaryDirectory $ \d ->
      forM_ [minBound .. maxBound] $ \typ -> do
         putStr $ show typ ++ " "
         hFlush stdout

         st <- mkStore d typ

         -- Run some tests on an empty store
         runRandomTests st

         replicateM 10 $ do
           putStr "."
           hFlush stdout

           -- Do some random operations to get the store into a
           -- different state
           n <- getRandomR (10, 500)
           ops <- replicateM n randomWriteOp
           forM_ ops $ \op -> applyOp op st

           runRandomTests st

  putStrLn "done.\nRunning store consistency tests"
  -- Test that completely randomized operations have the same
  -- observable state between different store types
  replicateM_ 100 $ do
         withRandomStores doRandomOperations
         putStr "."
         hFlush stdout

  putStrLn "\ndone. Tests passed."

