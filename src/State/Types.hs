{-# LANGUAGE OverloadedStrings #-}
module State.Types
    ( Comment(..)

    , ChapterId
    , chapterId
    , mkChapterId

    , CommentId
    , commentId
    , mkCommentId

    , State(..)
    , SessionId(..)
    , SessionInfo(..)
    )
where

import Control.Applicative ( (<$>), (<*>) )
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Binary ( Binary(..), Get )
import Data.Time.Clock.POSIX ( POSIXTime )
import qualified Data.ByteString as B
import Network.URI ( URI )

newtype CommentId = CommentId { commentId :: T.Text } deriving (Ord, Eq, Show)
instance Binary CommentId where
    get = CommentId . E.decodeUtf8 <$> get
    put = put . E.encodeUtf8 . commentId

newtype ChapterId = ChapterId { chapterId :: T.Text } deriving (Ord, Eq, Show)
instance Binary ChapterId where
    get = ChapterId . E.decodeUtf8 <$> get
    put = put . E.encodeUtf8 . chapterId

data SessionInfo = SessionInfo { siName :: !T.Text
                               , siEmail :: !(Maybe T.Text)
                               } deriving (Eq, Show)

data State =
    State
    { findComments       :: CommentId -> IO [Comment]
    , getCounts          :: Maybe ChapterId -> IO [(CommentId, Int)]
    , getLastInfo        :: SessionId -> IO (Maybe SessionInfo)
    , getChapterComments :: ChapterId -> IO [(CommentId, Comment)]
    , getChapterURI      :: ChapterId -> IO (Maybe URI)

    , addComment         :: CommentId -> Maybe ChapterId -> Comment -> IO ()
    , addChapter         :: ChapterId -> [CommentId] -> Maybe URI -> IO ()
    }

data Comment =
    Comment
    { cName    :: !T.Text
    , cComment :: !T.Text
    , cEmail   :: Maybe T.Text
    , cDate    :: !POSIXTime
    , cSession :: !SessionId
    } deriving (Show, Eq)

newtype SessionId =
    SessionId { sidBS :: B.ByteString }
    deriving ( Eq, Show, Ord )

instance Binary Comment where
    put (Comment name comment email date sid) =
        do putU8 name
           putU8 comment
           put (E.encodeUtf8 <$> email)
           put (realToFrac date :: Double)
           put $ sidBS sid
        where putU8 = put . E.encodeUtf8

    get = Comment
          <$> getU8
          <*> getU8
          <*> (fmap E.decodeUtf8 <$> get)
          <*> fmap realToFrac (get :: Get Double)
          <*> fmap SessionId get
        where getU8 = E.decodeUtf8 <$> get

-- These functions allow any text content as comment and chapter ids,
-- but we may want to be able to restrict these to allow only things
-- that could appear in the wild (or at least some length
-- restrictions).
mkCommentId :: T.Text -> Maybe CommentId
mkCommentId = Just . CommentId

mkChapterId :: T.Text -> Maybe ChapterId
mkChapterId = Just . ChapterId
