module Feed
    ( commentFeed
    )
where

import State.Types ( ChapterId, chapterId, CommentId, commentId, Comment(..) )

import Data.Maybe ( listToMaybe )
import Data.Time ( formatTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import System.Locale ( defaultTimeLocale )
import Network.URI ( URI(uriFragment) )
import qualified Data.Text as T
import qualified Text.Feed.Types as Feed
import qualified Text.Feed.Constructor as Feed

commentFeed :: Feed.FeedKind -> ChapterId -> [(CommentId, Comment)]
            -> URI
            -> Maybe Int -> Feed.Feed
commentFeed k chId cs docURI limit =
    Feed.withFeedTitle feedTitle $
    setFeedDate $
    Feed.withFeedItems items $
    Feed.newFeed k
    where
      itemURI cId = show $
                    docURI { uriFragment = ('#':) $ T.unpack $ commentId cId }
      items = map mkItem $ maybe id take limit cs
      commentDateStr = formatTime defaultTimeLocale "%c" .
                       posixSecondsToUTCTime . cDate
      setFeedDate = maybe id (Feed.withFeedDate . commentDateStr . snd) $
                    listToMaybe cs
      feedTitle = "Comments on " ++ (T.unpack $ chapterId chId)
      mkItem (cId, c) =
          Feed.withItemTitle ("Comment on " ++ (T.unpack $ commentId cId)) $
          Feed.withItemAuthor (T.unpack $ cName c) $
          Feed.withItemDescription (T.unpack $ cComment c) $
          Feed.withItemPubDate (commentDateStr c) $
          Feed.withItemLink (itemURI cId) $
          Feed.withItemId True (itemURI cId) $
          Feed.newItem k
