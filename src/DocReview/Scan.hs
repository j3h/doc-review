module DocReview.Scan
    ( showAnalysis )
where

import State.Types ( CommentId, ChapterId, chapterId, commentId )
import qualified Data.Text as T
import Data.List ( groupBy, sortBy )
import Data.Function ( on )

showAnalysis :: [(FilePath, [(Maybe ChapterId, [CommentId])])] -> [String]
showAnalysis chapters =
    concat [ concatMap showFile chapters
           , []:showDupes chapters
           ]
    where
      chLabel = showString "Chapter: " . T.unpack . chapterId
      showChapter (mChId, cIds) =
          maybe "(unnamed)" chLabel mChId:
          map (showString "  " . showCommentId) cIds

      showCommentId = T.unpack . commentId

      showFile (fn, chs) = [ ""
                           , replicate 50 '='
                           , "Analysis of " ++ show fn
                           , replicate 50 '-'
                           , ""
                           ] ++ concatMap showChapter chs

      dupes =
          filter ((> 1) . length) .
          groupBy ((==) `on` getCId) .
          sortBy (compare `on` getCId) .
          recs
          where
            recs fs = do
              (fn, chs) <- fs
              (mChId, cIds) <- chs
              cId <- cIds
              [(fn, mChId, cId)]

      getCId (_, _, cId) = cId

      showDupes files = case map (map getCId) $ dupes files of
                          [] -> ["No duplicate comment ids"]
                          ds -> "Duplicate comment ids found:":
                                concatMap showDupeSet ds
          where
            showDupeSet = map (showString "  " . show)
