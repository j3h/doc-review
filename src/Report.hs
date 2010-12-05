module Report
    ( genReport
    , analyzeFiles
    , ReportConfig(..)
    )
where

import Control.Applicative ( (<$>) )
import Control.Monad       ( foldM, (>=>) )
import Data.Monoid         ( Monoid(..) )
import System.IO           ( IOMode(ReadMode), withBinaryFile )
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

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
getStats = withContents $ foldMapComments stats

withContents :: (B.ByteString -> IO a) -> FilePath -> IO a
withContents f fn = withBinaryFile fn ReadMode $ B.hGetContents >=> f

data ReportConfig = ReportConfig [FilePath]

analyzeFiles :: ReportConfig -> IO CommentStats
analyzeFiles (ReportConfig filenames) =
  foldM (\s fn -> s `seq` (mappend s <$> getStats fn)) mempty filenames

genReport :: ReportConfig -> CommentStats -> String
genReport (ReportConfig args) allStats =
    unlines $
    [ "Usage statistics for: " ++ unwords args
    , shows total " comments"
    , ""
    , "Comment counts:"
    ] ++ commentLines ++
    [ ""
    , case Map.size $ csUsers allStats of
        1 -> "1 user contributing"
        n -> shows n " users contributing"
    , ""
    , "E-mail addresses:"
    ] ++ emailAddrs
    where
      total = sum $ Map.elems $ csTopicCount allStats
      showCommentStat (cid, n) =
          concat ["  ", T.unpack $ commentId cid, " ", show n]
      commentLines = map showCommentStat $ Map.toList $ csTopicCount allStats
      indent = showString "  "
      emailAddrs = map (indent . T.unpack) $ Set.toList $ csEmails allStats
