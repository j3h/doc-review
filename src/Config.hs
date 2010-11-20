{-| Configuration and command-line arguments -}
module Config
    ( SConfig(..)
    , ScanConfig(..)
    , ScanType(..)
    , Action(..)
    , parseArgs
    , usage
    )
where

import Control.Monad ( msum, guard, mplus )
import Data.Char ( isAlphaNum, isSpace )
import Network.URI ( parseRelativeReference, URI )
import Data.Maybe ( fromMaybe )
import System.Console.GetOpt

import State.Types ( State )
import qualified State.Mem ( new )
import qualified State.Disk ( new )
import qualified State.SQLite ( new )

type Usage = String

data Action = Help Usage
            | RunServer SConfig
            | Scan ScanConfig

usage :: Usage
usage = baseUsage

parseArgs :: [String] -> Either Usage Action
parseArgs [] = Left $ unlines [baseUsage, "Please specify an action"]
parseArgs (cmd:args) =
    case lookup cmd acts of
      Just (us, _, parse) ->
          if any (`elem` args) [ "--help", "-h" ]
          then Right $ Help us
          else parse args
      Nothing -> Left $ unlines [baseUsage, "Unknown action: " ++ show cmd]

data ActionSpec flg cfg
    = ActionSpec { asName   :: String
                 , asDescr  :: String
                 , asOpts   :: [OptDescr flg]
                 , asParse  :: flg -> Err (cfg -> cfg)
                 , asCfg    :: cfg
                 , asAction :: cfg -> Action
                 }

acts :: [(String, (String, String, [String] -> Either Usage Action))]
acts = [ act serverAction
       , act scanAction
       ]
    where
      act a = ( asName a
              , ( usageInfo (asDescr a) (asOpts a)
                , asDescr a
                , parseAction a
                )
              )

baseUsage :: String
baseUsage =
    unlines ("Commands:":actDoc ++ "":storeDocumentation 78)
    where
      actDoc = map ("  "++) $ twoColumn 76 $ map showAct $ acts
      showAct (cmd, (_, descr, _)) = (cmd, descr)

parseAction :: ActionSpec flg cfg -> [String] -> Either Usage Action
parseAction act args =
    let result =
            case getOpt Permute (asOpts act) args of
              (os, [], []) -> sequenceE $ map (asParse act) os
              (_, extra, es) ->
                  Err $ case extra of
                          [] -> es
                          _  -> ("Unknown arguments: " ++ show extra):es
    in case result of
         Err es -> Left $ unlines $ [ usageInfo (asDescr act) (asOpts act)
                                    , "Errors parsing arguments:"
                                    ] ++ es
         Val fs -> Right $ asAction act $ foldr ($) (asCfg act) fs

--------------------------------------------------
-- Server

serverAction :: ActionSpec ServerOption SConfig
serverAction =
    ActionSpec { asName   = "run"
               , asDescr  = "Run the HTTP server for Web-based document review"
               , asOpts   = serverOpts
               , asParse  = applyServerOption
               , asCfg    = defaultServerConfig
               , asAction = RunServer
               }

-- |The configurable settings for an instance of the comments server
data SConfig =
    SConfig
    { cfgStore       :: !(IO State) -- ^How state is stored
    , cfgPort        :: !Int        -- ^What TCP port to listen on
    , cfgLogDir      :: !FilePath   -- ^Where to put the log files
    , cfgHostName    :: !String     -- ^The hostname used by Snap
    , cfgContentDir  :: !FilePath   -- ^Where to look for the
                                    -- documents to index and content
                                    -- to serve. This is mapped to the
                                    -- root of the Web server.
    , cfgScanType    :: !ScanType   -- ^Whether to scan for updated
                                    -- ids in the content directory
    , cfgStaticDir   :: !(Maybe FilePath) -- ^Other static content to serve
    , cfgDefaultPage :: !(Maybe URI)
    -- ^Where to redirect to if no URL is requested
    , cfgRunAs       :: !(Maybe String)
    , cfgLogTo       :: !(Maybe FilePath)
    }

-- |Default  settings (don't  save state  on disk,  run on  port 3000,
-- store logs here, scan for ids on startup)
defaultServerConfig :: SConfig
defaultServerConfig =
    SConfig
    { cfgStore       = State.Mem.new
    , cfgPort        = 3000
    , cfgLogDir      = "."
    , cfgHostName    = "localhost"
    , cfgContentDir  = "content"
    , cfgStaticDir   = Nothing
    , cfgScanType    = ScanOnStartup
    , cfgDefaultPage = Nothing
    , cfgRunAs       = Nothing
    , cfgLogTo       = Nothing
    }

-- All of the defined options
data ServerOption = OStore String
                  | OPort String
                  | OLogDir String
                  | OHost String
                  | OContentDir String
                  | OStaticDir String
                  | OScan ScanType
                  | ODefaultPage String
                  | ORunAs String
                  | OLogTo FilePath
                    deriving Eq

-- |When/whether to scan for new commentable paragraphs in the content
-- directory
data ScanType = ScanOnStartup
              | NoScan
                deriving Eq

serverOpts :: [OptDescr ServerOption]
serverOpts =
    [ Option "s" ["store"] (ReqArg OStore "STORE")
      "The storage type. mem, fs:<dirname> or sqlite:<filename>"
    , Option "" ["no-scan"] (NoArg (OScan NoScan))
      "Do not update the chapter database at startup"
    , Option "" ["scan"] (NoArg (OScan ScanOnStartup))
      "Scan for updated content at server startup (default)"
    , Option "" ["static-dir"] (ReqArg OStaticDir "DIR")
      "Serve static files from this directory"
    , Option "" ["content-dir"] (ReqArg OContentDir "DIR")
      "Serve content from this directory"
    , Option "p" ["port"] (ReqArg OPort "PORT")
      "Listen on this port"
    , Option "" ["log-dir"] (ReqArg OLogDir "DIR")
      "Store the error and access logs in this directory"
    , Option "" ["host"] (ReqArg OHost "HOSTNAME")
      "Use this as the Web server host (not for binding address)"
    , Option "" ["default-page"] (ReqArg ODefaultPage "URL")
      "Where the server should redirect when the URL / is requested"
    , Option "" ["run-as"] (ReqArg ORunAs "USERNAME")
      "If started as root, attempt to drop privileges by \n\
      \changing to this user once the port is bound"
    , Option "" ["log-to"] (ReqArg OLogTo "LOGFILE")
      "Write a binary log file as a backup in case of primary\n\
      \store failure"
    ]

-- Turn an option string into something that updates the configuration
applyServerOption :: ServerOption -> Err (SConfig -> SConfig)
applyServerOption (OStore sType) =
    case parseStore sType of
      Nothing -> fail $ "Unknown store type: " ++ show sType
      Just st -> return $ \cfg -> cfg { cfgStore = st }
applyServerOption (OPort str) =
    case reads str of
      [(x, [])] -> return $ \cfg -> cfg { cfgPort = x }
      _         -> fail $ "Bad port: " ++ show str
applyServerOption (OLogDir str) = return $ \cfg -> cfg { cfgLogDir = str }
applyServerOption (OHost str) = return $ \cfg -> cfg { cfgHostName = str }
applyServerOption (OContentDir str) = return $ \cfg -> cfg { cfgContentDir = str }
applyServerOption (OStaticDir str) = return $
                               \cfg -> cfg { cfgStaticDir = Just str }
applyServerOption (OScan st) = return $ \cfg -> cfg { cfgScanType = st }
applyServerOption (ODefaultPage str) =
    case parseRelativeReference str of
      Nothing -> fail $ "Not a valid URI: " ++ show str
      Just u  -> return $ \cfg ->
                 cfg { cfgDefaultPage = Just u }
applyServerOption (ORunAs str) = return $ \cfg -> cfg { cfgRunAs = Just str }
applyServerOption (OLogTo str) = return $ \cfg -> cfg { cfgLogTo = Just str }

--------------------------------------------------
-- 'scan' action

scanAction :: ActionSpec ScanOption ScanConfig
scanAction =
    ActionSpec
    { asName   = "scan"
    , asDescr  = "Scan a set of documents for commentable \
                 \items and (optionally) update a store"
    , asOpts   = [ Option "" ["content-dir"] (ReqArg ScanContentDir "DIR")
                   "Scan this directory for content (default: \"content\")"
                 , Option "s" ["store"] (ReqArg ScanStore "STORE")
                   "The storage type. mem, fs:<dirname> or sqlite:<filename>"
                 ]

    , asParse  = applyScanOption
    , asCfg    = ScanConfig "content" Nothing
    , asAction = Scan
    }

data ScanConfig =
    ScanConfig
    { sCfgContentDir :: !(FilePath)
    , sCfgStore      :: !(Maybe (IO State))
    -- ^If this is Nothing, then just print out what we find.
    }

data ScanOption
    = ScanStore String
    | ScanContentDir FilePath
      deriving (Eq, Show)

applyScanOption :: ScanOption -> Err (ScanConfig -> ScanConfig)
applyScanOption (ScanStore sType) =
    case parseStore sType of
      Nothing -> fail $ "Unknown store type: " ++ show sType
      Just st -> return $ \cfg -> cfg { sCfgStore = Just st }
applyScanOption (ScanContentDir p) =
    return $ \cfg -> cfg { sCfgContentDir = p }

--------------------------------------------------
-- Helper code

-- Error monad (like either)
data Err a = Err [String] | Val a
instance Monad Err where
    return        = Val
    fail          = Err . return
    (Err s) >>= _ = Err s
    (Val x) >>= f = f x

sequenceE :: [Err a] -> Err [a]
sequenceE xs = case errs xs of
                 ([], as) -> Val as
                 (es, _)  -> Err es

-- Partition errors and successes
errs :: [Err a] -> ([String], [a])
errs (x:xs) = let (es, as) = errs xs
              in case x of
                   Err e -> (e ++ es, as)
                   Val x' -> (es, x':as)
errs [] = ([], [])

parseStore :: String -> Maybe (IO State)
parseStore sType = msum $ map ((`parseOneStore` sType) . sdParse) stores

data StoreDef a = SD { sdParse :: ParseStore a
                     , sdDesc :: String
                     }

data ParseStore a
    = PSConst String a
    | PSArg String String (String -> Maybe a)

parseOneStore :: ParseStore a -> String -> Maybe a
parseOneStore (PSConst c x) s = guard (c == s) >> return x
parseOneStore (PSArg pfx _ f) s =
    case break (== ':') s of
      (k, ':':s') | k == pfx -> f s'
      _                      -> Nothing

psArgStr :: String -> String -> (String -> a) -> ParseStore a
psArgStr pfx desc f = PSArg pfx desc $ Just . f

stores :: [ StoreDef (IO State) ]
stores = [ SD (PSConst "mem" State.Mem.new)
           "Use an ephemeral in-memory store (data will be lost when \
           \the process ends)"
         , SD (psArgStr "fs" "DIRECTORY" State.Disk.new)
           "Store data in flat files in the specified directory. This \
           \store is not ACID (has known race conditions and updates \
           \are not atomic)"
         , SD (psArgStr "sqlite" "DBNAME" State.SQLite.new)
           "Store data in a SQLite database specified by DBNAME. Uses \
           \the syntax of the SQLite API"
         ]

storeDocumentation :: Int -> [String]
storeDocumentation lineLength =
    wrap lineLength "Specifying data storage:" ++ showStores
    where
      showStores = map ("  "++) $
                   twoColumn (lineLength - 2) $
                   map showStore stores
      showStore sd = (showPS $ sdParse sd, sdDesc sd)
      showPS (PSConst s _) = s
      showPS (PSArg s a _) = showString s $ ':':a

--------------------------------------------------
-- Laying out documentation

-- |Lay out pairs of strings in two columns, under the specified line length
twoColumn :: Int -> [(String, String)] -> [String]
twoColumn w ps = concatMap fmt1 ps
    where
      firstColN = min (maximum $ map (length . fst) ps) 40

      secondColN = w - firstColN - 1 - 2

      fmt1 (a, b) = hcat firstColN secondColN a b

      hcat n1 n2 = go
          where
            go [] [] = []
            go xs ys =
                let (c1, xs') = findBreak n1 xs
                    (c2, ys') = findBreak n2 ys
                    l = fillCol n1 c1 ++ " " ++ fillCol n2 c2
                in l : go xs' ys'
      fillCol n xs = take n $ xs ++ repeat ' '

-- |Wrap a string at a certain line length, attempting to break at
-- word boundaries
wrap :: Int -> String -> [String]
wrap n = go
    where
      go s = case findBreak n s of
               ([], []) -> []
               (l,  s') -> l:go s'

-- |Attempt to break a string at a word boundary. As a last resort,
-- arbitrarily break at the specified line limit
findBreak :: Int -> String -> (String, String)
findBreak n s = let (l, s') = fromMaybe (splitAt n s) $ go 0 id s
                in (l, dropWhile isSpace s')
    where
      go l acc xs =
          let ok = return (acc [], xs)
          in case xs of
               []     -> ok
               (c:cs) ->
                      let next = go (l + 1) (acc . (c:)) cs
                      in case () of
                           () | l >= n       -> Nothing
                              | isAlphaNum c -> next
                              | otherwise    -> next `mplus` ok
