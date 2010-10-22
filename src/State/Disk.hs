module State.Disk
    ( new
    )
where

import Data.Maybe ( fromMaybe, mapMaybe )
import Control.Applicative ( (<$>) )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath ( (</>) )
import System.Directory ( getDirectoryContents, createDirectoryIfMissing )
import Control.Arrow ( second )
import qualified Data.ByteString.Lazy as B
import Data.Binary ( encode, decode )
import State.Types ( State(..), CommentId, commentId
                   , ChapterId, chapterId, mkCommentId, Comment(..)
                   )

new :: FilePath -> IO State
new p =
    let st = State { findComments = \cId -> readCommentsFile cId
                   , getCounts = \chId ->
                                 do cs <- readComments =<<
                                          maybe getAllCommentIds return =<<
                                          maybe (return Nothing) readChapterFile chId
                                    return $ map (second length) cs
                   , addComment =
                     \cId chId c -> do
                       case chId of
                         Nothing -> return ()
                         Just chap ->
                             do cIds <- fromMaybe [] <$> readChapterFile chap
                                writeChapterFile chap (cId:cIds)
                       cs <- readCommentsFile cId
                       writeCommentsFile cId (cs ++ [c])
                   }

        commentPath cId = p </> "comments" </> T.unpack (commentId cId)

        chapterPath chId = p </> "chapters" </> T.unpack (chapterId chId)

        readCommentsFile :: CommentId -> IO [Comment]
        readCommentsFile cId = tryRead `catch` \_ -> return []
            where tryRead = decode <$> B.readFile (commentPath cId)

        writeCommentsFile cId = B.writeFile (commentPath cId) . encode

        readChapterFile chId = tryRead `catch` \_ -> return Nothing
            where tryRead = do
                    content <- T.readFile $ chapterPath chId
                    return $ Just $ mapMaybe mkCommentId $ T.lines content

        writeChapterFile chId =
            T.writeFile (chapterPath chId) . T.unlines . map commentId

        getAllCommentIds =
          let ignoreFile f = (f `elem` [".", ".."])
          in mapMaybe (mkCommentId . T.pack) .
             filter (not . ignoreFile) <$>
             getDirectoryContents (p </> "comments")

        readComments cIds = zip cIds `fmap` mapM readCommentsFile cIds
    in do
      createDirectoryIfMissing True $ p </> "comments"
      createDirectoryIfMissing True $ p </> "chapters"
      return st
