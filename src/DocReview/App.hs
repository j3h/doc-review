{-# LANGUAGE OverloadedStrings #-}
module DocReview.App
where

import           Control.Applicative          ( (<$>), (<*>), (<|>) )
import           Control.Arrow                ( first )
import           Control.Monad                ( replicateM )
import           Control.Monad.IO.Class       ( liftIO )
import           Control.Monad.Random         ( getRandomR )
import           Data.Char                    ( toLower )
import           Data.Foldable                ( mapM_ )
import           Data.Maybe                   ( fromJust, fromMaybe )
import           Data.Time.Clock              ( getCurrentTime, addUTCTime )
import           Data.Time.Clock.POSIX        ( getPOSIXTime )
import           Network.URI                  ( URI
                                              , relativeTo
                                              , parseURI
                                              )
import           Prelude hiding               ( mapM_ )
import           Snap.Iteratee                ( enumBS )
import           Snap.Types                   ( Snap, finishWith, writeText
                                              , pass, dir, route, ifTop
                                              , redirect, withRequest
                                              , rqServerName, rqServerPort
                                              , rqIsSecure, rqCookies
                                              , getRequest, Cookie(..)
                                              , modifyResponse, addHeader
                                              , addCookie, setResponseStatus
                                              , writeBS, setResponseBody
                                              , Response, getParam, getResponse
                                              , emptyResponse
                                              )
import           Snap.Util.FileServe          ( fileServe )
import           System.FilePath              ( (</>) )
import           Text.XHtmlCombinators        ( render, XHtml, FlowContent )
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as E
import qualified Text.JSON             as JSON
import qualified Text.XML.Light.Output as XML
import qualified Text.Atom.Feed.Export as Atom

import           State.Types                  ( chapterId, commentId, State
                                              , ChapterId, CommentId
                                              , SessionId(..), mkChapterId
                                              , getCounts, mkCommentId
                                              , addComment, findComments
                                              , getChapterURI
                                              , getChapterComments
                                              , getLastInfo, Comment(..)
                                              )
import           Paths_doc_review             ( getDataFileName )
import           Server                       ( emptyServerConfig
                                              , hostname
                                              , accessLog
                                              , errorLog
                                              , port
                                              , server
                                              )
import           Privilege                    ( tryDropPrivilege )
import           DocReview.App.HTML           ( chronoView, getMarkup )
import           Feed                         ( commentFeed )
import qualified Config.Command.Run    as Run

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
