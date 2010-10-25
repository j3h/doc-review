module State.Disk
    ( new
    )
where

import Control.Applicative ( (<$>) )
import Control.Arrow       ( second )
import Data.Binary         ( encode, decode )
import Data.Maybe          ( fromMaybe, mapMaybe )
import System.Directory    ( getDirectoryContents, createDirectoryIfMissing )
import System.FilePath     ( (</>) )
import System.IO           ( withBinaryFile, IOMode(ReadMode) )
import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Encoding   as E
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C

import State.Types         ( State(..), CommentId, commentId , ChapterId
                           , chapterId, mkCommentId, Comment(..) )

-- Encode arbitrary text as a filename that is safe to use on a POSIX
-- filesystem (no special characters)
safe :: T.Text -> FilePath
safe = C.unpack . C.map safeChar . B64.encode . E.encodeUtf8
    where
      -- Convert the Base64 string to something that's OK to use on a
      -- POSIX filesystem by replacing '/' with '-'.
      safeChar '/' = '-'
      safeChar '-' = error "Unexpected '-' in base-64 encoded string"
      safeChar x   = x

new :: FilePath -> IO State
new storeDir =
    do createDirectoryIfMissing True commentsDir
       createDirectoryIfMissing True chaptersDir
       return $  State { findComments = findComments'

                       , getCounts =
                         \chId -> do
                           cs <- readComments =<<
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
                           cs <- findComments' cId
                           writeCommentsFile cId (cs ++ [c])
                       }
    where
      commentsDir = storeDir </> "comments"

      commentPath cId = commentsDir </> safe (commentId cId)

      readComments :: [CommentId] -> IO [(CommentId, [Comment])]
      readComments cIds = zip cIds `fmap` mapM findComments' cIds

      writeCommentsFile cId = B.writeFile (commentPath cId) . encode

      findComments' cId = tryRead `catch` \_ -> return []
          where
            tryRead = withBinaryFile (commentPath cId) ReadMode $ \h ->
                      do cs <- decode <$> B.hGetContents h
                         length cs `seq` return cs

      getAllCommentIds =
          mapMaybe (mkCommentId . T.pack) <$> getDirectoryContents commentsDir

      chaptersDir = storeDir </> "chapters"

      chapterPath chId = chaptersDir </> safe (chapterId chId)

      readChapterFile chId = tryRead `catch` \_ -> return Nothing
          where
            tryRead = do
              content <- T.readFile $ chapterPath chId
              return $ Just $ mapMaybe mkCommentId $ T.lines content

      writeChapterFile chId =
          T.writeFile (chapterPath chId) . T.unlines . map commentId
