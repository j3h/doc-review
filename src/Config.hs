{-| Configuration and command-line arguments -}
module Config
    ( SConfig(..)
    , ScanConfig(..)
    , ScanType(..)
    , Action(..)
    , parseArgs
    , serverOpts
    )
where

import Network.URI ( parseRelativeReference, URI )
import System.Console.GetOpt

import State.Types ( State )
import qualified State.Mem ( new )
import qualified State.Disk ( new )
import qualified State.SQLite ( new )

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

mkAction :: String
         -> (t -> Either [String] t2)
         -> [OptDescr a]
         -> (t2 -> Action)
         -> t
         -> Either (String -> String, [String]) Action
mkAction cmd par opts cons args =
    case par args of
      Left es -> Left (\pn -> usageInfo (pn ++ " " ++ cmd) opts, es)
      Right c -> Right $ cons c

parseArgs :: [String] -> Either (String -> String, [String]) Action
parseArgs [] = Left (id, ["Please specify an action"])
parseArgs args | any (`elem` args) [ "--help", "-h" ] = Right Help
parseArgs (cmd:opts) =
    case cmd of
      "server" -> mkAction cmd parseServerOptions serverOpts RunServer opts
      "scan"   -> mkAction cmd parseScanOptions scanOpts Scan opts
      "help"   -> Right Help
      _        -> Left (id, ["Unknown action: " ++ show cmd])

data ScanConfig =
    ScanConfig
    { sCfgContentDir :: !(FilePath)
    , sCfgStore      :: !(Maybe (IO State))
    -- ^If this is Nothing, then just print out what we find.
    }

defaultScanConfig :: ScanConfig
defaultScanConfig = ScanConfig "content" Nothing

data Action = Help
            | RunServer SConfig
            | Scan ScanConfig

-- |Default  settings (don't  save state  on disk,  run on  port 3000,
-- store logs here, scan for ids on startup)
defaultConfig :: SConfig
defaultConfig =
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

-- |When/whether to scan for new commentable paragraphs in the content
-- directory
data ScanType = ScanOnStartup
              | NoScan
                deriving Eq

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

data ScanOption
    = ScanStore String
    | ScanContentDir FilePath
      deriving (Eq, Show)

-- Error monad (like either)
data Err a = Err String | Val a
instance Monad Err where
    return        = Val
    fail          = Err
    (Err s) >>= _ = Err s
    (Val x) >>= f = f x

-- Partition errors and successes
errs :: [Err a] -> ([String], [a])
errs (x:xs) = let (es, as) = errs xs
              in case x of
                   Err e -> (e:es, as)
                   Val x' -> (es, x':as)
errs [] = ([], [])

parseStore :: String -> Err (IO State)
parseStore sType =
    case break (== ':') sType of
      ("mem", []) -> return $ State.Mem.new
      ("fs", (':':fsDir)) -> return $ State.Disk.new fsDir
      ("sqlite", (':':sFile)) -> return $ State.SQLite.new sFile
      _ -> fail $ "I don't understand store type: " ++ show sType

applyScanOption :: ScanOption -> Err (ScanConfig -> ScanConfig)
applyScanOption (ScanStore sType) = do
  st <- parseStore sType
  return $ \cfg -> cfg { sCfgStore = Just st }
applyScanOption (ScanContentDir p) =
    return $ \cfg -> cfg { sCfgContentDir = p }

-- Turn an option string into something that updates the configuration
applyServerOption :: ServerOption -> Err (SConfig -> SConfig)
applyServerOption (OStore sType) = do
  st <- parseStore sType
  return $ \cfg -> cfg { cfgStore = st }
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

parseOptionsG :: [OptDescr a] -> (a -> Err (c -> c)) -> c -> [String] -> Either [String] c
parseOptionsG optsG apply mt args =
    case getOpt Permute optsG args of
      (os, [], []) -> case errs $ map apply os of
                        ([], fs) -> Right $ foldr ($) mt fs
                        (es, _)  -> Left es
      (_, extra, es) ->
          Left $ case extra of
                   [] -> es
                   _  -> ("Unknown arguments: " ++ show extra):es

-- |Parse command-line arguments
parseServerOptions :: [String] -> Either [String] SConfig
                -- ^List of error messages or a configuration.
parseServerOptions =
    parseOptionsG serverOpts applyServerOption defaultConfig

parseScanOptions :: [String] -> Either [String] ScanConfig
parseScanOptions = parseOptionsG scanOpts applyScanOption defaultScanConfig

scanOpts :: [OptDescr ScanOption]
scanOpts =
    [ Option "" ["content-dir"] (ReqArg ScanContentDir "DIR")
      "Scan this directory for content (default: \"content\")"
    , Option "s" ["store"] (ReqArg ScanStore "STORE")
      "The storage type. mem, fs:<dirname> or sqlite:<filename>"
    ]

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
