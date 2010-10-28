{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ( (<$>), (<*>), (<|>) )
import           Control.Arrow ( first )
import           Control.Monad.IO.Class ( liftIO )
import           Data.Foldable ( mapM_ )
import           Data.Time.Clock.POSIX ( getPOSIXTime
                                       , posixSecondsToUTCTime, POSIXTime )
import           Data.Time.Format ( formatTime )
import           Prelude hiding (catch, mapM_)
import           Snap.Iteratee ( enumBS )
import           Snap.Types ( dir )
import           Snap.Types hiding (dir)
import           Snap.Util.FileServe ( fileServe )
import           System.Locale ( defaultTimeLocale )
import           Text.XHtmlCombinators
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Text.JSON as JSON
import qualified Text.XHtmlCombinators.Attributes as A

import           Server
import           State.Types
import qualified State.Disk ( new )
import qualified State.Mem ( new )
import qualified State.SQLite ( new )

main :: IO ()
main = do
  st <- State.SQLite.new "state.db"
  quickServer $
       dir "comments"
               (route [ ("single/:id", getCommentHandler st)
                      , ("chapter/:chapid/count/", getCountsHandler st)
                      , ("submit/:id", submitHandler st)
                      ]) <|>
       fileServe "static" <|>
       fileServe "content"

--------------------------------------------------
-- Handlers

getCountsHandler :: State -> Snap ()
getCountsHandler st = do
  chapId <- mkChapterId <$> requireParam "chapid"
  counts <- liftIO $ getCounts st chapId
  modifyResponse $ addHeader "content-type" "text/json"
  writeText $ T.pack $ JSON.encode $ countsJSON counts

countsJSON :: [(CommentId, Int)] -> JSON.JSValue
countsJSON = JSON.showJSON . JSON.toJSObject .
             map (first (T.unpack . commentId))

getCommentHandler :: State -> Snap ()
getCommentHandler st = do
  mcid <- mkCommentId <$> requireParam "id"
  case mcid of
    Nothing -> finishWith $ badRequest "Bad comment id"
    Just cid -> respondComments cid st

submitHandler :: State -> Snap ()
submitHandler st = do
  mcid <- mkCommentId <$> requireParam "id"
  case mcid of
    Nothing -> finishWith $ badRequest "Bad comment id"
    Just cid ->
        do comment <- Comment <$> requireParam "name"
                              <*> requireParam "comment"
                              <*> getParamUtf8 "email"
                              <*> liftIO getPOSIXTime
           chap <- (mkChapterId =<<) <$> getParamUtf8 "chapid"
           liftIO $ addComment st cid chap comment
           respondComments cid st

respondComments :: CommentId -> State -> Snap ()
respondComments cid st = do
  cs <- liftIO $ findComments st cid
  modifyResponse $ addHeader "content-type" "text/html"
  writeText $ render $ (getMarkup cid cs :: XHtml FlowContent)

badRequest :: T.Text -> Response
badRequest msg = setResponseBody (enumBS $ E.encodeUtf8 msg) $
                 setResponseStatus 400 "Bad request" $
                 emptyResponse

requireParam :: T.Text -> Snap T.Text
requireParam argName =
    getParamUtf8 argName >>=
    maybe (finishWith $ badRequest missing) return
    where
      missing = T.concat ["Missing parameter ", argName]

getParamUtf8 :: T.Text -> Snap (Maybe T.Text)
getParamUtf8 argName = fmap E.decodeUtf8 <$> getParam (E.encodeUtf8 argName)

--------------------------------------------------
-- Rendering comments

getMarkup :: Block a => CommentId -> [Comment] -> XHtml a
getMarkup cId cs = do
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
    _   -> mapM_ (commentMarkup cId) cs
  newCommentForm cId
  div_ $ span' [ A.class_ "comment_error" ] $ empty

newCommentForm :: Block a => CommentId -> XHtml a
newCommentForm cId =
    form' (addId "/comments/submit/")
              [A.id_ (addId "form_"), A.class_ "comment", A.method "post"] $ do
                span' [A.class_ "field-name"] $ text "Name: "
                input' [A.name "name", A.type_ "text"]
                br
                span' [A.class_ "field-name"] $ text "E-Mail Address: "
                input' [A.name "email", A.type_ "text"]
                text "(optional, will not be displayed)"
                br
                span' [A.class_ "field-name"] $ text "Comment: "
                br
                textarea' 10 40 [A.name "comment"] ""
                br
                input' [A.name "submit", A.type_ "submit"]
    where
      addId t = T.concat [t, commentId cId]

commentMarkup :: Block a => CommentId -> Comment -> XHtml a
commentMarkup _cId c =
    div' [A.class_ "comment"] $ do
      div' [A.class_ "username"] $
           do text $ cName c
              text " "
              span' [A.class_ "date"] $ text $ fmt $ cDate c
      div' [A.class_ "comment-text"] $ text $ cComment c

fmt :: POSIXTime -> T.Text
fmt = T.pack .
      formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC" .
      posixSecondsToUTCTime