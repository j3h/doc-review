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
import qualified Text.Atom.Feed as Atom

commentFeed :: ChapterId -> [(CommentId, Comment)] -> URI -> Maybe Int
            -> Atom.Feed
commentFeed chId cs docURI limit =
    (Atom.nullFeed (show chapterURI) (Atom.TextString feedTitle) feedDate)
    { Atom.feedEntries = map mkItem $ maybe id take limit cs
    , Atom.feedLinks   = [ alternateHtml $ chapterURI ]
    }
    where
      alternateHtml u = (Atom.nullLink u)
                        { Atom.linkRel = Just $ Left "alternate"
                        , Atom.linkType = Just "text/html"
                        }
      chapterURI = docFragText $ chapterId chId
      itemURI cId = docFragText $ commentId cId
      docFragText t = show $ docURI { uriFragment = '#':T.unpack t }
      commentDateStr = formatTime defaultTimeLocale "%c" .
                       posixSecondsToUTCTime . cDate
      feedDate = maybe "no-date" (commentDateStr . snd) $ listToMaybe cs
      feedTitle = "Comments on " ++ (T.unpack $ chapterId chId)

      mkItem (cId, c) =
          let -- XXX: this identifier should have a comment id to make
              -- this unique per comment instead of unique per
              -- commentable node
              iid    = itemURI cId

              ititle = Atom.TextString $
                       "Comment on " ++ (T.unpack $ commentId cId)

              idate  = commentDateStr c

              author = Atom.nullPerson
                       { Atom.personName = T.unpack $ cName c }

          in (Atom.nullEntry iid ititle idate)
                 { Atom.entryLinks   = [ alternateHtml $ itemURI cId ]
                 , Atom.entryAuthors = [ author ]
                 , Atom.entryContent =
                     Just $ Atom.TextContent $ T.unpack $ cComment c
                 }
