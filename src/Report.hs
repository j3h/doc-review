{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main )
where

import Control.Applicative ( (<$>) )
import Control.Monad ( foldM, forM_ )
import Data.Monoid ( Monoid(..) )
import System.Environment ( getArgs )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as Set
import qualified Data.Map as Map

import State.Logger ( foldMapComments )
import State.Types

data CommentStats =
    CommentStats
    { csTopicCount :: !(Map.Map CommentId Int)
    , csUsers      :: !(Map.Map SessionId Int)
    , csEmails     :: !(Set.Set T.Text)
    }

instance Monoid CommentStats where
    mempty = CommentStats mempty mempty mempty
    mappend stats1 stats2 =
        CommentStats (hist csTopicCount) (hist csUsers) (comb mappend csEmails)
            where
              hist :: Ord a => (CommentStats -> Map.Map a Int) -> Map.Map a Int
              hist = comb $ Map.unionWith (+)
              comb f field = f (field stats1) (field stats2)

stats :: CommentId -> a -> Comment -> CommentStats
stats cid _ c = CommentStats
                (Map.singleton cid 1)
                (Map.singleton (cSession c) 1)
                (maybe Set.empty Set.singleton $ cEmail c)

getStats :: FilePath -> IO CommentStats
getStats = foldMapComments stats

main :: IO ()
main = do
  args <- getArgs
  allStats <- foldM (\s fn -> s `seq` (mappend s <$> getStats fn)) mempty args
  putStrLn $ "Usage statistics for: " ++ unwords args
  let total = sum $ Map.elems $ csTopicCount allStats
  putStrLn $ "\n" ++ shows total " comments"
  T.putStrLn "\nComment counts:"
  forM_ (Map.toList $ csTopicCount allStats) $ \(cid, n) -> do
      T.putStr "  "
      T.putStr $ commentId cid
      T.putStr " "
      putStrLn $ show n

  putStr "\n"
  putStrLn $ case Map.size $ csUsers allStats of
               1 -> "1 user contributing"
               n -> shows n " users contributing"
  putStrLn "\nE-mail addresses:"
  forM_ (Set.toList $ csEmails allStats) $ T.putStrLn
