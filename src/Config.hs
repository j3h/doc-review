{-| Configuration and command-line arguments -}
module Config
    ( SConfig(..)
    , ScanConfig(..)
    , Action(..)
    , parseArgs
    , usage
    )
where

import Data.Monoid ( Monoid(..) )
import Control.Monad ( msum, guard, mplus, liftM, ap )
import Data.Char ( isAlphaNum, isSpace )
import Network.URI ( parseRelativeReference, URI )
import Data.Maybe ( fromMaybe )
import Control.Arrow ( left )
import System.Console.GetOpt

import State.Types ( State )
import qualified State.Mem ( new )
import qualified State.Disk ( new )
import qualified State.SQLite ( new )

newtype Usage = Usage [String]

instance Monoid Usage where
    mempty = Usage []
    mappend (Usage docs1) (Usage docs2) = Usage $ docs1 ++ ([]:docs2)

data Action = Help String
            | RunServer SConfig
            | Scan ScanConfig

usage :: Usage
usage = baseUsage

parseArgs :: [String] -> Either String Action
parseArgs = left mkUsage . parseArgs'

isHelpRequest :: [String] -> Bool
isHelpRequest args = any (`elem` args) ["-h", "--help"]

parseArgs' :: [String] -> Either Usage Action
parseArgs' [] = Left $ baseUsage `mappend`
                Usage ["Please specify a command"]
parseArgs' args =
    case msum $ map tryParse acts of
      Just res -> res
      Nothing  ->
          if isHelpRequest args
          then Right $ Help $ mkUsage baseUsage
          else Left $ baseUsage `mappend`
               Usage ["Unknown command: " ++ show args]
    where
      tryParse (_, (_, p)) = p args

data ActionSpec flg cfg
    = ActionSpec { asName   :: String
                 , asDescr  :: String
                 , asOpts   :: [OptDescr (Err (cfg -> cfg))]
                 , asCfg    :: [String] -> Err cfg
                 , asAction :: cfg -> Action
                 }

actionUsage :: ActionSpec flg cfg -> Usage
actionUsage a = Usage [usageInfo (unlines $ wrap 78 $ asDescr a) (asOpts a)]

acts :: [(String, (String, [String] -> Maybe (Either Usage Action)))]
acts = [ act serverAction
       , act scanAction
       ]
    where
      act a = ( asName a
              , ( asDescr a
                , parseAction a
                )
              )

mkUsage :: Usage -> String
mkUsage (Usage docs) =
    unlines $
    concat
    [ wrap 78 "Web-based document review system, based on the system \
              \used for Real World Haskell \
              \<http://book.realworldhaskell.org/>"
    , []:docs
    ]

baseUsage :: Usage
baseUsage =
    Usage $ concat $
    [ "Commands:":actionDocumentation
    , ["", "Use <command> --help for help on any individual command."]
    , "":storeDocumentation 78
    ]
    where
      actionDocumentation = indent 2 $ twoColumn 76 $ map showAct $ acts
      showAct (cmd, (descr, _)) = (cmd, descr)

parseAction :: ActionSpec flg cfg -> [String] -> Maybe (Either Usage Action)
parseAction act args =
    let (os, args', es) = getOpt Permute (asOpts act) args
        flgs = if null es
               then sequenceE os
               else Err $ map strip es ++ toErrors (sequenceE os)
        strip = reverse . dropWhile isSpace . reverse
        result xs = case (flgs, asCfg act xs) of
                      (Err es1, Err es2) -> Err $ es1 ++ es2
                      (_, cfg)           -> foldr ($) `liftM` cfg `ap` flgs
        handle (Err msgs)  =
            Left $ actionUsage act `mappend`
                 Usage ("Errors parsing arguments:":indent 2 msgs)
        handle (Val cfg) =
            Right $ asAction act cfg
    in case args' of
         (k:actArgs) | k == asName act ->
                         Just $ if isHelpRequest actArgs
                                then Right $ Help $ mkUsage $ actionUsage act
                                else handle $ result actArgs
         _                             -> Nothing

--------------------------------------------------
-- Server

serverAction :: ActionSpec ServerOption SConfig
serverAction =
    ActionSpec
    { asName   = "run"
    , asDescr  =
      "Run the HTTP server for Web-based document review, that can serve \
      \the static content, the AJAX API for adding and displaying comments, \
      \and the Atom feed and HTML comment viewing API"
    , asOpts   = serverOpts
    , asCfg    =
      noArgs $
      SConfig
      { cfgStore       = State.Mem.new
      , cfgPort        = 3000
      , cfgLogDir      = "."
      , cfgHostName    = "localhost"
      , cfgContentDir  = "content"
      , cfgStaticDir   = Nothing
      , cfgScanOnStart = True
      , cfgDefaultPage = Nothing
      , cfgRunAs       = Nothing
      , cfgLogTo       = Nothing
      }
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
    , cfgScanOnStart :: !Bool       -- ^Whether to scan for updated
                                    -- ids in the content directory
    , cfgStaticDir   :: !(Maybe FilePath) -- ^Other static content to serve
    , cfgDefaultPage :: !(Maybe URI)
    -- ^Where to redirect to if no URL is requested
    , cfgRunAs       :: !(Maybe String)
    , cfgLogTo       :: !(Maybe FilePath)
    }

-- All of the defined options
type ServerOption = Err (SConfig -> SConfig)

serverOpts :: [OptDescr ServerOption]
serverOpts =
    [ Option "s" ["store"] (ReqArg soptStore "STORE")
      "The storage type."
    , Option "" ["no-scan"] (NoArg (soptScan False))
      "Do not update the chapter database at startup"
    , Option "" ["scan"] (NoArg (soptScan True))
      "Scan for updated content at server startup (default)"
    , Option "" ["static-dir"] (ReqArg soptStaticDir "DIR")
      "Serve static files from this directory"
    , Option "" ["content-dir"] (ReqArg soptContentDir "DIR")
      "Serve content from this directory"
    , Option "p" ["port"] (ReqArg soptPort "PORT")
      "Listen on this port"
    , Option "" ["log-dir"] (ReqArg soptLogDir "DIR")
      "Store the error and access logs in this directory"
    , Option "" ["host"] (ReqArg soptHost "HOSTNAME")
      "Use this as the Web server host (not for binding address)"
    , Option "" ["default-page"] (ReqArg soptDefaultPage "URL")
      "Where the server should redirect when the URL / is requested"
    , Option "" ["run-as"] (ReqArg soptRunAs "USERNAME")
      "If started as root, attempt to drop privileges by \n\
      \changing to this user once the port is bound"
    , Option "" ["log-to"] (ReqArg soptLogTo "LOGFILE")
      "Write a binary log file as a backup in case of primary\n\
      \store failure"
    ]

-- Turn an option string into something that updates the configuration
soptStore, soptPort, soptLogDir, soptHost,
 soptContentDir, soptStaticDir, soptDefaultPage,
 soptRunAs, soptLogTo :: String -> ServerOption
soptStore           = storeOpt $ \st cfg -> cfg { cfgStore      = st }
soptLogDir str      = return   $ \cfg -> cfg { cfgLogDir     = str }
soptHost str        = return   $ \cfg -> cfg { cfgHostName   = str }
soptContentDir str  = return   $ \cfg -> cfg { cfgContentDir = str }
soptStaticDir str   = return   $ \cfg -> cfg { cfgStaticDir  = Just str }
soptRunAs str       = return   $ \cfg -> cfg { cfgRunAs      = Just str }
soptLogTo str       = return   $ \cfg -> cfg { cfgLogTo      = Just str }
soptPort str        =
    case reads str of
      [(x, [])] -> return $ \cfg -> cfg { cfgPort = x }
      _         -> fail $ "Bad port: " ++ show str
soptDefaultPage str =
    case parseRelativeReference str of
      Nothing -> fail $ "Not a valid URI: " ++ show str
      Just u  -> return $ \cfg ->
                 cfg { cfgDefaultPage = Just u }

soptScan :: Bool -> ServerOption
soptScan st = return $ \cfg -> cfg { cfgScanOnStart = st }

--------------------------------------------------
-- 'scan' action

scanAction :: ActionSpec ScanOption ScanConfig
scanAction =
    ActionSpec
    { asName   = "scan"
    , asDescr  = "Scan a set of documents for commentable \
                 \items and (optionally) update a store"
    , asOpts   = [ Option "" ["content-dir"] (ReqArg scanSetContentDir "DIR")
                   "Scan this directory for content (default: \"content\")"
                 , Option "s" ["store"] (ReqArg scanSetStore "STORE")
                   "The storage type. mem, fs:<dirname> or sqlite:<filename>"
                 ]
    , asCfg    = noArgs $ ScanConfig "content" Nothing
    , asAction = Scan
    }

data ScanConfig =
    ScanConfig
    { sCfgContentDir :: !(FilePath)
    , sCfgStore      :: !(Maybe (IO State))
    -- ^If this is Nothing, then just print out what we find.
    }

type ScanOption = Err (ScanConfig -> ScanConfig)

scanSetContentDir :: FilePath -> ScanOption
scanSetContentDir p = return $ \cfg -> cfg { sCfgContentDir = p }

scanSetStore :: String -> ScanOption
scanSetStore = storeOpt $ \st cfg -> cfg { sCfgStore = Just st }

--------------------------------------------------
-- Helper code

noArgs :: a -> [String] -> Err a
noArgs x [] = return x
noArgs _ args = fail $ "Unexpected arguments: " ++ show args

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

toErrors :: Err a -> [String]
toErrors (Err es) = es
toErrors _        = []

--------------------------------------------------
-- Creating a new store from the command-line flag

storeOpt :: Monad m => (IO State -> a -> a) -> String -> m (a -> a)
storeOpt set sType =
    case parseStore sType of
      Nothing -> fail $ "Unknown store type: " ++ show sType
      Just st -> return $ set st

parseStore :: String -> Maybe (IO State)
parseStore sType = msum $ map (`psParse` sType) stores

data ParseStore a = PS { psParse :: String -> Maybe a
                       , psKey   :: String
                       , psDescr :: String
                       }

psConst :: String -> a -> String -> ParseStore a
psConst k v = PS f k
    where
      f sType = guard (sType == k) >> return v

psArg :: String -> String -> (String -> Maybe a) -> String -> ParseStore a
psArg pfx argName p = PS f (pfx ++ ':':argName)
    where
      f sType = case break (== ':') sType of
                  (k, (':':rest)) | k == pfx -> p rest
                  _                          -> Nothing

psArgStr :: String -> String -> (String -> a) -> String -> ParseStore a
psArgStr pfx argName f = psArg pfx argName (Just . f)

stores :: [ ParseStore (IO State) ]
stores = [ psConst "mem" State.Mem.new
           "Use an ephemeral in-memory store (data will be lost when \
           \the process ends)"
         , psArgStr "fs" "DIRECTORY" State.Disk.new
           "Store data in flat files in the specified directory. This \
           \store is not ACID (has known race conditions and updates \
           \are not atomic)"
         , psArgStr "sqlite" "DBNAME" State.SQLite.new
           "Store data in a SQLite database specified by DBNAME. Uses \
           \the syntax of the SQLite API"
         ]

storeDocumentation :: Int -> [String]
storeDocumentation lineLength =
    wrap lineLength "Specifying data storage:" ++ showStores
    where
      showStores = indent 2 $
                   twoColumn (lineLength - 2) $
                   map showStore stores
      showStore ps = (psKey ps, psDescr ps)

--------------------------------------------------
-- Laying out documentation

-- |Lay out pairs of strings in two columns, under the specified line length
twoColumn :: Int -> [(String, String)] -> [String]
twoColumn w ps = concatMap fmt1 ps
    where
      firstColN = min (maximum $ map (length . fst) ps) 40

      secondColN = w - firstColN - 2

      fmt1 (a, b) = hcat firstColN secondColN a b

      hcat n1 n2 = go
          where
            go [] [] = []
            go xs ys =
                let (c1, xs') = findBreak n1 xs
                    (c2, ys') = findBreak n2 ys
                    l = fillCol n1 c1 ++ "  " ++ fillCol n2 c2
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

indent :: Int -> [String] -> [String]
indent n = map $ showString $ replicate n ' '