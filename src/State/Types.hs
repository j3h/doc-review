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
    , cEmail :: Maybe T.Text
    } deriving (Show, Eq)

instance Binary Comment where
    put (Comment name comment email) =
        putU8 name >> putU8 comment >> put (E.encodeUtf8 <$> email)
        where putU8 = put . E.encodeUtf8

    get = Comment <$> getU8 <*> getU8 <*> (fmap E.decodeUtf8 <$> get)
        where getU8 = E.decodeUtf8 <$> get

-- These functions allow any text content as comment and chapter ids,
-- but we may want to be able to restrict these to allow only things
-- that could appear in the wild (or at least some length
-- restrictions).
mkCommentId :: T.Text -> Maybe CommentId
mkCommentId = Just . CommentId

mkChapterId :: T.Text -> Maybe ChapterId
mkChapterId = Just . ChapterId
