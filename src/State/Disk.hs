{- WARNING: this store implementation has race-conditions for
   multi-threaded access! Its operations are also not atomic. Beware!
 -}

module State.Disk
    ( new
    )
where

import Control.Applicative ( (<$>) )
import Control.Arrow       ( second )
import Control.Monad       ( when )
import Data.Binary         ( encode, decode )
import Data.Maybe          ( fromMaybe, mapMaybe )
import System.Directory    ( getDirectoryContents, createDirectoryIfMissing )
import System.FilePath     ( (</>) )
import System.IO           ( withBinaryFile, IOMode(ReadMode, WriteMode) )
import Data.Bits           ( (.&.), shiftR )
import Data.Char           ( chr )
import Data.Time.Clock.POSIX ( POSIXTime )
import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Encoding   as E
import qualified Data.ByteString      as BS
import Data.Word ( Word8 )
import Data.List ( nub )
import Numeric ( readHex )

import State.Types         ( State(..), CommentId, commentId , ChapterId
                           , chapterId, mkCommentId, Comment(..)
                           , SessionId(..), SessionInfo(..) )

safe :: T.Text -> FilePath
safe = safeBS . E.encodeUtf8

-- Encode arbitrary text as a filename that is safe to use on a POSIX
-- filesystem (no special characters)
safeBS :: BS.ByteString -> FilePath
safeBS = concatMap makeSafe . BS.unpack
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

decodeFileName :: FilePath -> T.Text
decodeFileName = T.pack . go
    where
      go ('-':x:y:cs) = case readHex [x, y] of
                          [(n, [])] -> chr n:go cs
                          _ -> '-':go (x:y:cs)
      go (c:cs) = c:go cs
      go [] = []

new :: FilePath -> IO State
new storeDir =
    do createDirectoryIfMissing True commentsDir
       createDirectoryIfMissing True chaptersDir
       createDirectoryIfMissing True sessionsDir
       return $  State { findComments = findComments'

                       , getCounts =
                         \mChId -> do
                           cIds <- case mChId of
                                     Nothing -> getAllCommentIds
                                     Just chId -> maybe [] id <$> readChapterFile chId
                           cs <- readComments cIds
                           return $ filter ((> 0) . snd) $ map (second length) cs

                       , addComment =
                         \cId chId c -> do
                           case chId of
                             Nothing -> return ()
                             Just chap -> addChapter' chap [cId]
                           cs <- findComments' cId
                           writeCommentsFile cId (cs ++ [c])
                           writeSession (cSession c) (cName c)
                                            (cEmail c) (cDate c)

                       , addChapter = addChapter'


                       , getLastInfo =
                         \sId -> fmap (\(n, e, _) -> SessionInfo n e)
                                 `fmap` readSession sId
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
          mapMaybe (mkCommentId . decodeFileName) . filter okId
                       <$> getDirectoryContents commentsDir
              where
                okId = not . (`elem` [".", ".."])

      chaptersDir = storeDir </> "chapters"

      chapterPath chId = chaptersDir </> safe (chapterId chId)

      addChapter' chId cIds = do
        cIds' <- fromMaybe [] <$> readChapterFile chId
        writeChapterFile chId (nub $ cIds ++ cIds')

      readChapterFile chId = (Just `fmap` tryRead) `catch` \_ -> return Nothing
          where
            tryRead = do
              content <- T.readFile $ chapterPath chId
              return $ mapMaybe mkCommentId $ T.lines content

      writeChapterFile chId =
          T.writeFile (chapterPath chId) . T.unlines . map commentId

      sessionsDir = storeDir </> "sessions"

      sessionPath sid = sessionsDir </> safeBS (sidBS sid)

      writeSession sid n e d = do
        exists <- readSession sid
        let overwrite = case exists of
                          Nothing -> True
                          Just (_, _, d') -> d > d'
        when overwrite $
             withBinaryFile (sessionPath sid) WriteMode $ \h ->
                 B.hPut h $ encode ( E.encodeUtf8 n
                                   , fmap E.encodeUtf8 e
                                   , realToFrac d :: Double
                                   )

      readSession :: SessionId -> IO (Maybe (T.Text, Maybe T.Text, POSIXTime))
      readSession sid = (Just `fmap` readOK) `catch` \_ -> return Nothing
          where
            readOK =
                withBinaryFile (sessionPath sid) ReadMode $ \h -> do
                  (nBS, eBS, dD) <- decode <$> B.hGetContents h
                  return ( E.decodeUtf8 nBS
                         , fmap E.decodeUtf8 eBS
                         , realToFrac (dD :: Double)
                         )
