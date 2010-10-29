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
    )
where

import Control.Applicative ( (<$>), (<*>) )
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Binary ( Binary(..), Get )
import Data.Time.Clock.POSIX ( POSIXTime )

newtype CommentId = CommentId { commentId :: T.Text } deriving (Ord, Eq, Show)
newtype ChapterId = ChapterId { chapterId :: T.Text } deriving (Ord, Eq, Show)

data State =
    State
    { findComments :: CommentId -> IO [Comment]
    , getCounts :: Maybe ChapterId -> IO [(CommentId, Int)]
    , addComment :: CommentId -> Maybe ChapterId -> Comment -> IO ()
    , addChapter :: ChapterId -> [CommentId] -> IO ()
    }

data Comment =
    Comment
    { cName :: !T.Text
    , cComment :: !T.Text
    , cEmail :: Maybe T.Text
    , cDate :: !POSIXTime
    } deriving (Show, Eq)

instance Binary Comment where
    put (Comment name comment email date) =
        do putU8 name
           putU8 comment
           put (E.encodeUtf8 <$> email)
           put (realToFrac date :: Double)
        where putU8 = put . E.encodeUtf8

    get = Comment
          <$> getU8
          <*> getU8
          <*> (fmap E.decodeUtf8 <$> get)
          <*> fmap realToFrac (get :: Get Double)
        where getU8 = E.decodeUtf8 <$> get

-- These functions allow any text content as comment and chapter ids,
-- but we may want to be able to restrict these to allow only things
-- that could appear in the wild (or at least some length
-- restrictions).
mkCommentId :: T.Text -> Maybe CommentId
mkCommentId = Just . CommentId

mkChapterId :: T.Text -> Maybe ChapterId
mkChapterId = Just . ChapterId
