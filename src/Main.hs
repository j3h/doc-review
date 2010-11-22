{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative          ( (<$>), (<*>), (<|>) )
import           Control.Arrow                ( first )
import           Control.Monad                ( replicateM, guard, when )
import           Control.Monad.IO.Class       ( liftIO )
import           Control.Monad.Random         ( getRandomR )
import           Data.Char                    ( toLower )
import           Data.Foldable                ( mapM_, forM_ )
import           Data.Maybe                   ( fromJust, fromMaybe )
import           Data.Time.Clock              ( getCurrentTime, addUTCTime )
import           Data.Time.Clock.POSIX        ( getPOSIXTime
                                              , posixSecondsToUTCTime )
import           Data.Time.Format             ( formatTime )
import           Data.Function                ( on )
import           Data.List                    ( sortBy, groupBy )
import           Network.URI                  ( URI(uriFragment)
                                              , relativeTo
                                              , parseURI
                                              , parseRelativeReference
                                              )
import           Prelude hiding               ( mapM_ )
import           Snap.Iteratee                ( enumBS )
import           Snap.Types
import           Snap.Util.FileServe          ( fileServe )
import           System.Environment           ( getArgs )
import           System.Exit                  ( exitFailure, exitSuccess )
import           System.FilePath              ( (</>) )
import           System.Locale                ( defaultTimeLocale )
import           Text.XHtmlCombinators
import           Text.XHtmlCombinators.Escape ( escape, escapeAttr )
import qualified Data.ByteString.Char8            as B
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as E
import qualified Text.JSON                        as JSON
import qualified Text.XHtmlCombinators.Attributes as A
import qualified Text.XML.Light.Output            as XML
import qualified Text.Atom.Feed.Export            as Atom

import           Config           ( parseArgs, unUsage, Usage, Action(..) )
import qualified Config.Command.Run as Run
import qualified Config.Command.Scan as Scan
import           Analyze          ( analyze )
import           Privilege        ( tryDropPrivilege )
import           Server
import           State.Types
import qualified State.Logger as L
import           Paths_doc_review ( getDataFileName )
import           Feed ( commentFeed )

showUsage :: Usage -> IO ()
showUsage = putStr . unUsage 78

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left usg ->
        do showUsage usg
           exitFailure

    Right (Help usg) ->
        do showUsage usg
           exitSuccess

    Right (RunServer cfg) ->
        do st <- maybe return L.wrap (Run.cfgLogTo cfg) =<< Run.cfgStore cfg
           when (Run.cfgScanOnStart cfg) $ scanDir cfg st
           runServer cfg st

    Right (Scan cfg) ->
        let cDir = Scan.contentDir cfg

            showChapter (mChId, cIds) =
                maybe "(unnamed)" (showString "Chapter: " . T.unpack . chapterId) mChId:
                map (showString "  " . showCommentId) cIds

            showCommentId = (T.unpack . commentId)

            showFile (fn, chs) = [ ""
                                 , replicate 50 '='
                                 , "Analysis of " ++ show fn
                                 , replicate 50 '-'
                                 , ""
                                 ] ++ concatMap showChapter chs

            dupes =
                  filter ((> 1) . length) .
                  groupBy ((==) `on` getCId) .
                  sortBy (compare `on` getCId) .
                  recs
                where
                  recs fs = do
                        (fn, chs) <- fs
                        (mChId, cIds) <- chs
                        cId <- cIds
                        [(fn, mChId, cId)]

            getCId (_, _, cId) = cId

            showDupes files = case map (map getCId) $ dupes files of
                                [] -> ["No duplicate comment ids"]
                                ds -> "Duplicate comment ids found:":
                                      concatMap showDupeSet ds
                where
                  showDupeSet = map (showString "  " . show)

        in case Scan.store cfg of
             Just mk -> loadChapters cDir =<< mk
             Nothing ->
                 do files <- analyze cDir
                    putStr $ unlines $ concatMap showFile files
                    putStr $ unlines $ []:showDupes files

-- |Scan the content directory for chapter HTML files and update the
-- database to include all of the chapter mappings
loadChapters :: FilePath -> State -> IO ()
loadChapters chapterDir st = do
  chapters <- analyze chapterDir
  storeChapters chapters st

storeChapters :: [(String, [(Maybe ChapterId, [CommentId])])]
              -> State -> IO ()
storeChapters files st = do
  forM_ files $ \(fn, chapters) -> do
      let uri = parseRelativeReference fn
      forM_ chapters $ \(mChId, cIds) ->
          maybe (return ()) (\chId -> addChapter st chId cIds uri) mChId

runServer :: Run.Config -> State -> IO ()
runServer cfg st = do
  static <- case Run.cfgStaticDir cfg of
              Nothing -> getDataFileName "static"
              Just s  -> return s

  let hostBS = E.encodeUtf8 $ T.pack $ Run.cfgHostName cfg
      sCfg = emptyServerConfig
             { hostname = hostBS
             , accessLog = Just $ Run.cfgLogDir cfg </> "access.log"
             , errorLog = Just $ Run.cfgLogDir cfg </> "error.log"
             , port = Run.cfgPort cfg
             }

  dropPriv <- maybe (return (return ())) tryDropPrivilege $ Run.cfgRunAs cfg
  let dropPrivAct = liftIO dropPriv >> pass
  server sCfg $ dropPrivAct <|> trackSession (app static cfg st)

scanDir :: Run.Config -> State -> IO ()
scanDir = loadChapters . Run.cfgContentDir

app :: FilePath -> Run.Config -> State -> SessionId -> Snap ()
app static cfg st sessionId =
    dir "comments"
            (route [ ("single/:id", getCommentHandler st sessionId)
                   , ("chapter/:chapid/count/", getCountsHandler st)
                   , ("chapter/:chapid/feed", getChapterFeedHandler sessionId st)
                   , ("submit/:id", submitHandler st sessionId)
                   ]) <|>
    fileServe (Run.cfgContentDir cfg) <|>
    fileServe static <|>
    maybe pass (ifTop . relativeRedirect) (Run.cfgDefaultPage cfg)

-- |Redirect to the supplied URI (relative to the current request's
-- URI)
relativeRedirect :: URI -> Snap ()
relativeRedirect d = do
  u <- baseURL
  -- relativeTo's implementation always returns Just
  redirect $ B.pack $ show $ fromJust $ d `relativeTo` u

-- |The base URL for this request (taking into account the protocol,
-- the host, and the port)
baseURL :: Snap URI
baseURL =
    withRequest $ \req -> do
      let host = B.unpack $ rqServerName req

          rport = rqServerPort req

          defaultPort | rqIsSecure req = 443
                      | otherwise      = 80

          proto | rqIsSecure req = "https"
                | otherwise = "http"

          authStr | rport == defaultPort || rport == 0 = host
                  | otherwise = host ++ ':':show rport

      case parseURI $ proto ++ "://" ++ authStr of

        Nothing -> finishWith $ badRequest $
                   T.concat ["Bad host header: ", T.pack host]

        Just u  -> return u

newSessionId :: IO SessionId
newSessionId = SessionId . B.pack <$> replicateM 24 selectRandomSessionChar
    where
      selectRandomSessionChar =
          B.index sessionChars <$> getRandomR (0, B.length sessionChars - 1)

sessionChars :: B.ByteString
sessionChars = B.pack $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "."

trackSession :: (SessionId -> Snap a) -> Snap a
trackSession act = do
  let sessionCookieName = "doc-review-session"
  cookies <- rqCookies <$> getRequest
  sessionId <-
      case [c | c <- cookies, cookieName c == sessionCookieName ] of
        []    -> liftIO newSessionId
        (c:_) -> return $ SessionId $ cookieValue c

  res <- act sessionId

  -- Expires in 1000000 seconds
  expiry <- addUTCTime 1000000 <$> liftIO getCurrentTime

  let newCookie =
          Cookie { cookieName = sessionCookieName
                 , cookieValue = let SessionId sid = sessionId
                                 in sid
                 , cookieExpires = Just expiry
                 , cookieDomain = Nothing
                 , cookiePath = Just "/"
                 }

  modifyResponse $ addCookie newCookie

  return res

--------------------------------------------------
-- Handlers

getChapterFeedHandler :: SessionId -> State -> Snap ()
getChapterFeedHandler sId st = do
  Just chapId <- mkChapterId <$> requireParam "chapid"
  makeAbsolute <- flip relativeTo <$> baseURL
  chapURL <- liftIO $ getChapterURI st chapId
  case chapURL >>= makeAbsolute of
    Nothing -> do
      modifyResponse $ setResponseStatus 404 "Not Found"
                     . addHeader "content-type" "text/plain"
      mapM_ writeBS [ "Chapter not found: "
                    , E.encodeUtf8 $ chapterId chapId
                    ]
      finishWith =<< getResponse
    Just u -> do
      limit <- fromMaybe 10 <$> getLimitParam "limit"
      cs <- liftIO $ take limit <$> getChapterComments st chapId
      fmt <- fromMaybe AtomFeed <$> getFormatParam "format"
      case fmt of
        AtomFeed ->
            do modifyResponse $ addHeader "content-type" "application/atom+xml"
               writeText $ T.pack $ XML.showTopElement $
                         Atom.xmlFeed $ commentFeed chapId cs u
        WebPage ->
            do modifyResponse $ addHeader "content-type" "text/html"
               writeText $ render $ chronoView sId chapId cs u

getParamM :: (B.ByteString -> Maybe a) -> B.ByteString -> Snap (Maybe a)
getParamM f name = (f =<<) <$> getParam name

getLimitParam :: B.ByteString -- ^Parameter name
              -> Snap (Maybe Int)
getLimitParam = getParamM $ \lim ->
                case reads $ B.unpack lim of
                  [(n, [])] -> Just n
                  _         -> Nothing -- log something here
                                       -- because of the bad parse?

data Format = AtomFeed | WebPage

getFormatParam :: B.ByteString -> Snap (Maybe Format)
getFormatParam = getParamM $ (`lookup` fs) . B.map toLower
    where fs = [ ("atom", AtomFeed), ("html", WebPage) ]

getCountsHandler :: State -> Snap ()
getCountsHandler st = do
  chapId <- mkChapterId <$> requireParam "chapid"
  counts <- liftIO $ getCounts st chapId
  modifyResponse $ addHeader "content-type" "text/json"
  writeText $ T.pack $ JSON.encode $ countsJSON counts

countsJSON :: [(CommentId, Int)] -> JSON.JSValue
countsJSON = JSON.showJSON . JSON.toJSObject .
             map (first (T.unpack . commentId))

getCommentHandler :: State -> SessionId -> Snap ()
getCommentHandler st sessionId = do
  mcid <- mkCommentId <$> requireParam "id"
  case mcid of
    Nothing -> finishWith $ badRequest "Bad comment id"
    Just cid -> respondComments cid st sessionId

submitHandler :: State -> SessionId -> Snap ()
submitHandler st sessionId = do
  mcid <- mkCommentId <$> requireParam "id"
  case mcid of
    Nothing -> finishWith $ badRequest "Bad comment id"
    Just cid ->
        do comment <- Comment <$> requireParam "name"
                              <*> requireParam "comment"
                              <*> getParamUtf8 "email"
                              <*> liftIO getPOSIXTime
                              <*> return sessionId
           chap <- (mkChapterId =<<) <$> getParamUtf8 "chapid"
           liftIO $ addComment st cid chap comment
           respondComments cid st sessionId

--------------------------------------------------
-- Snap helpers

respondComments :: CommentId -> State -> SessionId -> Snap ()
respondComments cid st sessionId = do
  cs <- liftIO $ findComments st cid
  si <- liftIO $ getLastInfo st sessionId
  modifyResponse $ addHeader "content-type" "text/html"
  writeText $ render $ (getMarkup cid cs si sessionId :: XHtml FlowContent)

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

chronoView :: SessionId -> ChapterId -> [(CommentId, Comment)] -> URI -> XHtml Page
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
