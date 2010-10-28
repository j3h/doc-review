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
import Data.Bits           ( (.&.), shiftR )
import Data.Char           ( chr )
import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Encoding   as E
import qualified Data.ByteString      as BS
import Data.Word ( Word8 )

import State.Types         ( State(..), CommentId, commentId , ChapterId
                           , chapterId, mkCommentId, Comment(..) )

-- Encode arbitrary text as a filename that is safe to use on a POSIX
-- filesystem (no special characters)
safe :: T.Text -> FilePath
safe = concatMap makeSafe . BS.unpack . E.encodeUtf8
    where
      -- Conservative set of characters that are allowed unescaped in
      -- a filename
      unescaped w = if or [ '0' |-| '9', 'a' |-| 'z', 'A' |-| 'Z' ]
                    then Just c
                    else Nothing
          where
            c = chr $ fromIntegral w
            lo |-| hi = c >= lo && c <= hi

      -- Keep characters that are allowed unescaped, and use '-' as an
      -- escape sequence, followed by two hex digits for octets that
      -- are out of range.
      makeSafe :: Word8 -> [Char]
      makeSafe c = maybe ('-':hex c) return $ unescaped c

      -- Convert the Word8 to a pair of hex digits
      hex c = map hDig [c `shiftR` 4, c .&. 0x0f]

      -- Convert the four-bit value into a hex digit
      hDig = ((['0'..'9'] ++ ['a'..'f']) !!) . fromIntegral

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
