module Config.Command.Run
    ( Config(..)
    , mkCfg
    , opts
    )
where

import State.Types ( State )
import Network.URI ( parseRelativeReference, URI )
import Config.GetOpt ( MkCfg, Opts, noArgs, Err )
import Config.Store ( storeOptDescr, ParseStore )
import System.Console.GetOpt
import qualified State.Mem ( new )

-- |The configurable settings for an instance of the comments server
data Config =
    Config
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

mkCfg :: MkCfg Config
mkCfg =
    noArgs $
    Config
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

-- All of the defined options
opts :: [ParseStore (IO State)] -> Opts Config
opts stores =
    [ storeOptDescr stores $ \st cfg -> cfg { cfgStore = st }
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

type Opt = Err (Config -> Config)

-- Turn an option string into something that updates the configuration
soptPort, soptLogDir, soptHost, soptContentDir, soptStaticDir,
 soptDefaultPage, soptRunAs, soptLogTo :: String -> Opt
soptLogDir str      = return $ \cfg -> cfg { cfgLogDir     = str }
soptHost str        = return $ \cfg -> cfg { cfgHostName   = str }
soptContentDir str  = return $ \cfg -> cfg { cfgContentDir = str }
soptStaticDir str   = return $ \cfg -> cfg { cfgStaticDir  = Just str }
soptRunAs str       = return $ \cfg -> cfg { cfgRunAs      = Just str }
soptLogTo str       = return $ \cfg -> cfg { cfgLogTo      = Just str }
soptPort str        =
    case reads str of
      [(x, [])] -> return $ \cfg -> cfg { cfgPort = x }
      _         -> fail $ "Bad port: " ++ show str
soptDefaultPage str =
    case parseRelativeReference str of
      Nothing -> fail $ "Not a valid URI: " ++ show str
      Just u  -> return $ \cfg ->
                 cfg { cfgDefaultPage = Just u }

soptScan :: Bool -> Opt
soptScan st = return $ \cfg -> cfg { cfgScanOnStart = st }
