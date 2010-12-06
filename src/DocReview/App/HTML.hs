{-# LANGUAGE OverloadedStrings #-}
module DocReview.App.HTML
    ( chronoView
    , getMarkup
    , commentMarkup
    )
where

import Data.Time.Clock.POSIX        ( posixSecondsToUTCTime )
import Data.Time.Format             ( formatTime )
import State.Types                  ( SessionId, ChapterId, CommentId, Comment
                                    , SessionInfo, chapterId, commentId, cName
                                    , cSession, cDate, cComment, siName
                                    , siEmail )
import System.Locale                ( defaultTimeLocale )
import Network.URI                  ( URI(uriFragment) )
import Control.Monad                ( guard, forM_ )
import Text.XHtmlCombinators.Escape ( escape, escapeAttr )
import Text.XHtmlCombinators
    ( XHtml, Page, Block, html, head_, title, link', text, body, div', h1, a'
    , p, div_, span', empty, form', table, tbody, tr, th, label', td
    , textarea', span_, input'
    )

import qualified Data.Text as T
import qualified Text.XHtmlCombinators.Attributes as A

-- |Render a Web page with a chronological listing of comments,
-- linking back to the original document.
chronoView :: SessionId -> ChapterId -> [(CommentId, Comment)] -> URI
           -> XHtml Page
chronoView sId chId cs u =
    html True $ do
      head_ $ do
        title $ pageTitle
        link' [ A.type_ "text/css"
              , A.href $ escape "/styles.css"
              , A.rel "stylesheet" ]
      body $ div' [ A.id_ "content" ] $
           do div' [ A.class_ "navheader" ] $ h1 $ text pageTitle
              div' [ A.class_ "chapter" ] $ do
                  p $ do
                    text $ escape $ T.concat
                             [ "This is a chronological listing of comments on "
                             , chapterId chId
                             , ". Click on each comment's link to view the "
                             , "comment in context. You can also view them "
                             , "inline using the links "
                             ]
                    a' [ A.href $ escapeAttr $ T.pack $ show u ] $ text $
                       escape $ "within the document itself"
                    text $ escape "."
                  forM_ cs $ \(cId, c) ->
                      commentMarkup sId cId (Just u) c
    where
      pageTitle = escape $ T.concat [ "Chronological view of comments on "
                                    , chapterId chId
                                    ]

-- |Render the comments for a single commentable item, along with a
-- form to submit new comments.
getMarkup :: Block a => CommentId -> [Comment] -> Maybe SessionInfo
          -> SessionId -> XHtml a
getMarkup cId cs si sid = do
  div' [ A.id_ $ T.concat ["toggle_", commentId cId], A.class_ "toggle" ] $
       a' [ A.class_ "commenttoggle"
          , A.attr "onclick" $
            T.concat ["return toggleComment('", commentId cId, "')"]
          , A.href "show/hide comments"
          ] $ text $ case length cs of
                       0 -> "No comments"
                       1 -> "1 comment"
                       n -> T.concat [T.pack (show n), " comments"]
  case cs of
    []  -> do
      div' [A.class_ "comment"] $
           text "Be the first to comment on this paragraph!"
      return ()
    _   -> mapM_ (commentMarkup sid cId Nothing) cs
  newCommentForm cId si
  div_ $ span' [ A.class_ "comment_error" ] $ empty

-- |Render a form to add a new comment to the specified item.
newCommentForm :: Block a => CommentId -> Maybe SessionInfo -> XHtml a
newCommentForm cId msi =
    form' (addId "/comments/submit/") [ idAttr "form_"
                                      , A.class_ "comment"
                                      , A.method "post"
                                      ] $
    table $ tbody $ do
      tr $ do
        th $ label' [forAttr "comment_"] $ text "Comment:"
        td $ textarea' 10 40 [A.name "comment", idAttr "comment_"] ""
      tr $ do
        th $ label' [forAttr "name_"] $ text "Name:"
        td $ input' [ A.name "name"
                    , A.type_ "text"
                    , idAttr "name_"
                    , nameValue
                    ]
      tr $ do
        th $ label' [forAttr "email_"] $ text "E-Mail Address: "
        td $ do
             input' [ A.name "email"
                    , A.type_ "text"
                    , idAttr "email_"
                    , emailValue
                    ]
             text " (optional, will not be displayed)"
      tr $ do
        td empty
        td $ input' [A.name "submit", A.type_ "submit"]
    where
      nameValue = A.value $ maybe "" (escapeAttr . siName) msi
      emailValue = A.value $ maybe "" escapeAttr $ siEmail =<< msi
      forAttr t = A.for (addId t)
      idAttr t = A.id_ (addId t)
      addId t = T.concat [t, commentId cId]

commentMarkup :: Block a => SessionId -> CommentId -> Maybe URI -> Comment
              -> XHtml a
commentMarkup sid cId mURI c =
    div' [A.class_ topCls] $ do
      div' [A.class_ "username"] $ maybeLink $
           do text $ escape $ name
              text " "
              span' [A.class_ "date"] $ text $ escape $ fmtTime $ cDate c
      div' [A.class_ "comment-text"] $ text $ escape $ cComment c
    where
      maybeLink = maybe span_ (toAnchor . addFrag) mURI
      toAnchor u = a' [ A.href $ escapeAttr $ T.pack $ show u ]
      addFrag u = u { uriFragment = '#':T.unpack (commentId cId) }
      isMine = sid == cSession c
      name | isMine = T.concat [ cName c, " (you)" ]
           | otherwise = cName c
      topCls = T.unwords $ ["comment"] ++ (guard isMine >> return "mine")
      fmtTime = T.pack .
                formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC" .
                posixSecondsToUTCTime
