{-| Configuration and command-line arguments -}
module Config
    ( Config(..)
    , ScanType(..)
    , parseOptions
    , opts
    )
where

import System.Console.GetOpt

import State.Types ( State )
import qualified State.Mem ( new )
import qualified State.Disk ( new )
import qualified State.SQLite ( new )

-- |The configurable settings for an instance of the comments server
data Config =
    Config
    { cfgStore      :: !(IO State) -- ^How state is stored
    , cfgPort       :: !Int        -- ^What TCP port to listen on
    , cfgLogDir     :: !FilePath   -- ^Where to put the log files
    , cfgHostName   :: !String     -- ^The hostname used by Snap
    , cfgContentDir :: !FilePath   -- ^Where to look for the documents
                                   -- to index and content to
                                   -- serve. This is mapped to the
                                   -- root of the Web server.
    , cfgScanType   :: !ScanType   -- ^Whether to scan for updated ids
                                   -- in the content directory
    , cfgStaticDir  :: !FilePath   -- ^Other static content to serve
    }

-- |Default  settings (don't  save state  on disk,  run on  port 3000,
-- store logs here, scan for ids on startup)
defaultConfig :: Config
defaultConfig =
    Config
    { cfgStore      = State.Mem.new
    , cfgPort       = 3000
    , cfgLogDir     = "."
    , cfgHostName   = "localhost"
    , cfgContentDir = "content"
    , cfgStaticDir  = "static"
    , cfgScanType   = ScanOnStartup
    }

-- |When/whether to scan for new commentable paragraphs in the content
-- directory
data ScanType = ScanOnStartup
              | ScanOnly
              | NoScan
                deriving Eq

-- All of the defined options
data Option = OStore String
            | OPort String
            | OLogDir String
            | OHost String
            | OContentDir String
            | OStaticDir String
            | OScan ScanType
            | OHelp
              deriving Eq

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

-- Turn an option string into something that updates the configuration
applyOption :: Option -> Err (Config -> Config)
applyOption (OStore sType) = do
  st <- case break (== ':') sType of
          ("mem", []) -> return $ State.Mem.new
          ("fs", (':':fsDir)) -> return $ State.Disk.new fsDir
          ("sqlite", (':':sFile)) -> return $ State.SQLite.new sFile
          _ -> fail $ "I don't understand store type: " ++ show sType
  return $ \cfg -> cfg { cfgStore = st }
applyOption (OPort str) =
    case reads str of
      [(x, [])] -> return $ \cfg -> cfg { cfgPort = x }
      _         -> fail $ "Bad port: " ++ show str
applyOption (OLogDir str) = return $ \cfg -> cfg { cfgLogDir = str }
applyOption (OHost str) = return $ \cfg -> cfg { cfgHostName = str }
applyOption (OContentDir str) = return $ \cfg -> cfg { cfgContentDir = str }
applyOption (OStaticDir str) = return $ \cfg -> cfg { cfgStaticDir = str }
applyOption (OScan st) = return $ \cfg -> cfg { cfgScanType = st }
applyOption OHelp = return id

-- |Parse command-line arguments
parseOptions :: [String]
             -> Either [String] (Maybe Config) -- ^List of error
                                               -- messages or a
                                               -- configuration. Nothing
                                               -- means asking for
                                               -- help.
parseOptions args =
    case getOpt Permute opts args of
      (os, _, _) | OHelp `elem` os -> Right Nothing
      (os, [], []) -> case errs $ map applyOption os of
                        ([], fs) -> Right $ Just $ foldr ($) defaultConfig fs
                        (es, _)  -> Left es
      (_, extra, es) ->
          Left $ case extra of
                   [] -> es
                   _  -> ("Unknown arguments: " ++ show extra):es

opts :: [OptDescr Option]
opts = [ Option "s" ["store"] (ReqArg OStore "STORE")
         "The storage type. mem, fs:<dirname> or sqlite:<filename>"
       , Option "" ["no-scan"] (NoArg (OScan NoScan))
         "Do not update the chapter database at startup"
       , Option "" ["only-scan"] (NoArg (OScan ScanOnly))
         "Update the chapter database and do not run the server"
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
       , Option "" ["help"] (NoArg OHelp)
         "Get usage"
       ]
