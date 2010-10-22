{-# LANGUAGE OverloadedStrings #-}
module State
    ( Comment(..)
    , State(..)
    , CommentId
    , commentId
    , mkCommentId
    , ChapterId
    , chapterId
    , mkChapterId
    , newMemState
    , newDiskState
    )
where

import Data.Char ( isAlphaNum )
import Data.Maybe ( fromMaybe, mapMaybe )
import Control.Applicative ( (<$>), (<*>) )
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as T
import Data.Foldable ( toList )
import System.FilePath ( (</>) )
import System.Directory ( getDirectoryContents, createDirectoryIfMissing )
import Control.Concurrent.MVar ( newMVar, withMVar, modifyMVar_ )
import Control.Arrow ( second )
import Data.Binary ( Binary(..), encode, decode )
import qualified Data.ByteString.Lazy as B

data Comment = Comment { cName :: T.Text
                       , cComment :: T.Text
                       } deriving (Show, Eq)

mkCommentId :: T.Text -> Maybe CommentId
mkCommentId = fmap CommentId . safeId

mkChapterId :: T.Text -> Maybe ChapterId
mkChapterId = fmap ChapterId . safeId

clean :: T.Text -> T.Text
clean =
    T.intercalate "-" .
    filter (not . T.null) .
    T.splitBy (\c -> not $ isAlphaNum c)

safeId :: T.Text -> Maybe T.Text
safeId t = let s = clean t
           in if T.null s
              then Nothing
              else Just s

newtype CommentId = CommentId { commentId :: T.Text } deriving (Ord, Eq)
newtype ChapterId = ChapterId { chapterId :: T.Text } deriving (Ord, Eq)

data State =
 State
 { findComments :: CommentId -> IO [Comment]
 , getCounts :: Maybe ChapterId -> IO [(CommentId, Int)]
 , addComment :: CommentId -> Maybe ChapterId -> Comment -> IO ()
 }

newMemState :: IO State
newMemState = do
  v <- newMVar (Map.empty, Map.empty)
  return $ State
             { findComments =
               \cId ->
                   withMVar v $
                   return . maybe [] toList . Map.lookup cId . snd

             , getCounts =
               \chId ->
                   withMVar v $ \(chs, cms) ->
                   return $ map (second S.length) $ Map.toList $
                   case chId >>= (`Map.lookup` chs) of
                     Nothing   -> cms
                     Just cIds -> Map.filterWithKey (\k _ -> k `Set.member` cIds) cms

             , addComment =
               \cId chId c ->
                   modifyMVar_ v $ \(chs, cms) ->
                       let chs' = case chId of
                                    Nothing -> chs
                                    Just chap ->
                                        Map.alter (addSet cId) chap chs
                           cms' = Map.alter (addSeq c) cId cms
                           addSet x = return . (Set.insert x) . maybe Set.empty id
                           addSeq x = return . (S.|> x) . maybe S.empty id
                       in return (chs', cms')
             }

newDiskState :: FilePath -> IO State
newDiskState p =
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

        commentPath (CommentId cId) = p </> "comments" </> T.unpack cId

        chapterPath (ChapterId chId) = p </> "chapters" </> T.unpack chId

        readCommentsFile :: CommentId -> IO [Comment]
        readCommentsFile cId = tryRead `catch` \_ -> return []
            where tryRead = decode <$> B.readFile (commentPath cId)

        writeCommentsFile cId = B.writeFile (commentPath cId) . encode

        readChapterFile chId = tryRead `catch` \_ -> return Nothing
            where tryRead = do
                    content <- T.readFile $ chapterPath chId
                    return $ Just $ map CommentId $ T.lines content

        writeChapterFile chId =
            T.writeFile (chapterPath chId) . T.unlines . map commentId

        getAllCommentIds =
          let ignoreFile f = (f `elem` [".", ".."])
          in mapMaybe (fmap CommentId . safeId . T.pack) .
             filter (not . ignoreFile) <$>
             getDirectoryContents (p </> "comments")

        readComments cIds = zip cIds `fmap` mapM readCommentsFile cIds
    in do
      createDirectoryIfMissing True $ p </> "comments"
      createDirectoryIfMissing True $ p </> "chapters"
      return st

instance Binary Comment where
    put (Comment name comment) =
        do put $ E.encodeUtf8 name
           put $ E.encodeUtf8 comment
    get = Comment <$> fmap E.decodeUtf8 get <*> fmap E.decodeUtf8 get
