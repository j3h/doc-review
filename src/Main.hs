{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ( (<$>), (<*>), (<|>) )
import           Control.Arrow ( first )
import           Control.Monad.IO.Class ( liftIO )
import           Data.Foldable ( mapM_, forM_ )
import           Data.Time.Clock.POSIX ( getPOSIXTime
                                       , posixSecondsToUTCTime )
import           Data.Time.Format ( formatTime )
import           Prelude hiding (catch, mapM_)
import           Snap.Iteratee ( enumBS )
import           Snap.Types ( dir )
import           Snap.Types hiding (dir)
import           Snap.Util.FileServe ( fileServe )
import           System.Console.GetOpt ( usageInfo )
import           System.Environment ( getArgs, getProgName )
import           System.Exit ( exitFailure, exitSuccess )
import           System.FilePath ( (</>) )
import           System.Locale ( defaultTimeLocale )
import           Text.XHtmlCombinators
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Text.JSON as JSON
import qualified Text.XHtmlCombinators.Attributes as A

import           Config ( Config(..), ScanType(..), parseOptions, opts, Action(..) )
import           Analyze ( analyzeDirectory )
import           Server
import           State.Types
import           Paths_doc_review ( getDataFileName )

usage :: IO String
usage = do
  pn <- getProgName
  return $ unlines $
             [ "Document review Web application, based on\
               \ http://book.realworldhaskell.org/"
             , ""
             , "Usage: " ++ pn ++ " [options]"
             , usageInfo pn opts
             ]

main :: IO ()
main = do
  args <- getArgs
  cfg <-
      case parseOptions args of
        Left es -> do
          putStr =<< usage
          putStrLn $ unlines es
          exitFailure
        Right Help -> do
          putStr =<< usage
          exitSuccess
        Right (Run c) -> return c

  st <- cfgStore cfg

  let sc = scanDir cfg st
      run = runServer cfg st
  case cfgScanType cfg of
    ScanOnly      -> sc
    ScanOnStartup -> sc >> run
    NoScan        -> run

-- |Scan the content directory for chapter HTML files and update the
-- database to include all of the chapter mappings
loadChapters :: FilePath -> State -> IO ()
loadChapters chapterDir st = do
  chapters <- analyzeDirectory chapterDir
  forM_ chapters $ \(mChId, cIds) ->
      maybe (return ()) (\chId -> addChapter st chId cIds) mChId

runServer :: Config -> State -> IO ()
runServer cfg st = do
  static <- case cfgStaticDir cfg of
              Nothing -> getDataFileName "static"
              Just s  -> return s

  let hostBS = E.encodeUtf8 $ T.pack $ cfgHostName cfg
      sCfg = emptyServerConfig
             { hostname = hostBS
             , accessLog = Just $ cfgLogDir cfg </> "access.log"
             , errorLog = Just $ cfgLogDir cfg </> "error.log"
             , port = cfgPort cfg
             }

  server sCfg $ app static cfg st

scanDir :: Config -> State -> IO ()
scanDir = loadChapters . cfgContentDir

app :: FilePath -> Config -> State -> Snap ()
app static cfg st =
    dir "comments"
            (route [ ("single/:id", getCommentHandler st)
                   , ("chapter/:chapid/count/", getCountsHandler st)
                   , ("submit/:id", submitHandler st)
                   ]) <|>
    fileServe (cfgContentDir cfg) <|>
    fileServe static <|>
    maybe pass (ifTop . redirect) (cfgDefaultPage cfg)

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

--------------------------------------------------
-- Snap helpers

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
        td $ input' [A.name "name", A.type_ "text", idAttr "name_"]
      tr $ do
        th $ label' [forAttr "email_"] $ text "E-Mail Address: "
        td $ do
             input' [A.name "email", A.type_ "text", idAttr "email_"]
             text " (optional, will not be displayed)"
      tr $ do
        td empty
        td $ input' [A.name "submit", A.type_ "submit"]
    where
      forAttr t = A.for (addId t)
      idAttr t = A.id_ (addId t)
      addId t = T.concat [t, commentId cId]

commentMarkup :: Block a => CommentId -> Comment -> XHtml a
commentMarkup _cId c =
    div' [A.class_ "comment"] $ do
      div' [A.class_ "username"] $
           do text $ cName c
              text " "
              span' [A.class_ "date"] $ text $ fmtTime $ cDate c
      div' [A.class_ "comment-text"] $ text $ cComment c
    where
      fmtTime = T.pack .
                formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC" .
                posixSecondsToUTCTime
