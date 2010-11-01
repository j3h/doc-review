{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative ( (<$>), (<*>), (<|>) )
import           Control.Arrow ( first )
import           Control.Monad ( replicateM, guard, unless, join )
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Random ( getRandomR )
import           Data.Foldable ( mapM_, forM_ )
import           Data.Time.Clock ( getCurrentTime, addUTCTime )
import           Data.Time.Clock.POSIX ( getPOSIXTime
                                       , posixSecondsToUTCTime )
import           Data.Time.Format ( formatTime )
import           Data.Maybe ( fromJust )
import           Prelude hiding (mapM_)
import           Network.URI ( URI, relativeTo, parseURI )
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
import           Text.XHtmlCombinators.Escape ( escape, escapeAttr )
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Text.JSON as JSON
import qualified Text.XHtmlCombinators.Attributes as A
import qualified Data.ByteString.Char8 as B
import           System.Posix.User ( getUserEntryForName, setUserID, userID
                                   , getRealUserID )
import           Data.IORef ( newIORef, readIORef, writeIORef )

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

  dropPriv <- tryDropPrivilege cfg
  server sCfg $ dropPriv <|> trackSession (app static cfg st)

-- Obtain an action that will attempt to drop privileges if the
-- environment and configuration allow it. Ideally, we would be able
-- to drop privileges after binding to the port and before doing
-- anything else. This is a compromise that provides a modicum of
-- safety if the request can co-opt the server, but no protection from
-- any local attacks. In particular, all of the server start-up stuff
-- happened before this, including creating the database. This could
-- potentially cause permission errors.
tryDropPrivilege :: Config -> IO (Snap ())
tryDropPrivilege cfg = do
  -- Figure out what user id we need to switch to (if any)
  uid <- getRealUserID
  targetUID <-
      case cfgRunAs cfg of
        Nothing -> return Nothing
        Just username ->
            do t <- userID <$> getUserEntryForName username
               case uid of
                 -- Root:
                 0            ->
                     return $ Just t

                 -- We already are the target user:
                 _ | t == uid ->
                     return Nothing

                 -- Anyone else:
                 _            ->
                     error $ "Only root can change users (trying to run as " ++ username ++ ")"

  case targetUID of
    Nothing     ->
        -- If we have no target UID, then just return a no-op
        return $ pass

    Just target ->
        -- Build and return an action that will ensure that we are
        -- running as the target user before proceeding
        do
          -- The variable that holds the action that ensures that we
          -- have dropped privileges
          startRef <- newIORef $ return ()

          -- Put the privilege dropping action in
          writeIORef startRef $
            do putStrLn $ "Dropping privileges: switching to user " ++ show target

               setUserID target `catch` \e ->
                   -- Check that we didn't lose a race
                   -- trying to drop privileges. If we lost,
                   -- then everything's OK because the
                   -- privileges are already dropped.
                   do newUID <- getRealUserID

                      -- If it wasn't losing the race,
                      -- raise it again here
                      unless (newUID == target) $ ioError e

               finalUID <- getRealUserID
               if finalUID == target
                 -- replace the action with a no-op
                 then writeIORef startRef $ return ()
                 else error "Failed to drop privileges"

          return $ do liftIO $ join $ readIORef startRef
                      -- Go on to the next handler
                      pass

scanDir :: Config -> State -> IO ()
scanDir = loadChapters . cfgContentDir

app :: FilePath -> Config -> State -> SessionId -> Snap ()
app static cfg st sessionId =
    dir "comments"
            (route [ ("single/:id", getCommentHandler st sessionId)
                   , ("chapter/:chapid/count/", getCountsHandler st)
                   , ("submit/:id", submitHandler st sessionId)
                   ]) <|>
    fileServe (cfgContentDir cfg) <|>
    fileServe static <|>
    maybe pass (ifTop . relativeRedirect) (cfgDefaultPage cfg)

-- |Redirect to the supplied URI (relative to the current request's
-- URI)
relativeRedirect :: URI -> Snap ()
relativeRedirect d =
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

        Just u  ->
            -- relativeTo's implementation always returns Just
            redirect $ B.pack $ show $ fromJust $ d `relativeTo` u

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
    _   -> mapM_ (commentMarkup sid cId) cs
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

commentMarkup :: Block a => SessionId -> CommentId -> Comment -> XHtml a
commentMarkup sid _cId c =
    div' [A.class_ topCls] $ do
      div' [A.class_ "username"] $
           do text $ escape $ name
              text " "
              span' [A.class_ "date"] $ text $ escape $ fmtTime $ cDate c
      div' [A.class_ "comment-text"] $ text $ escape $ cComment c
    where
      isMine = sid == cSession c
      name | isMine = T.concat [ cName c, " (you)" ]
           | otherwise = cName c
      topCls = T.unwords $ ["comment"] ++ (guard isMine >> return "mine")
      fmtTime = T.pack .
                formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC" .
                posixSecondsToUTCTime
