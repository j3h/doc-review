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
import Data.Binary ( Binary(..) )

newtype CommentId = CommentId { commentId :: T.Text } deriving (Ord, Eq)
newtype ChapterId = ChapterId { chapterId :: T.Text } deriving (Ord, Eq)

data State =
    State
    { findComments :: CommentId -> IO [Comment]
    , getCounts :: Maybe ChapterId -> IO [(CommentId, Int)]
    , addComment :: CommentId -> Maybe ChapterId -> Comment -> IO ()
    }

data Comment =
    Comment
    { cName :: !T.Text
    , cComment :: !T.Text
    } deriving (Show, Eq)

instance Binary Comment where
    put (Comment name comment) =
        do put $ E.encodeUtf8 name
           put $ E.encodeUtf8 comment
    get = Comment <$> fmap E.decodeUtf8 get <*> fmap E.decodeUtf8 get

mkCommentId :: T.Text -> Maybe CommentId
mkCommentId = Just . CommentId

mkChapterId :: T.Text -> Maybe ChapterId
mkChapterId = Just . ChapterId
