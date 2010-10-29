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

data Config = Config { cfgStore :: !(IO State)
                     , cfgPort :: !Int
                     , cfgLogDir :: !FilePath
                     , cfgHostName :: !String
                     , cfgContentDir :: !FilePath
                     , cfgScanType :: !ScanType
                     , cfgStaticDir :: !FilePath
                     }

defaultConfig :: Config
defaultConfig =
    Config
    { cfgStore = State.Mem.new
    , cfgPort = 3000
    , cfgLogDir = "."
    , cfgHostName = "localhost"
    , cfgContentDir = "content"
    , cfgStaticDir = "static"
    , cfgScanType = ScanOnStartup
    }

data ScanType = ScanOnStartup
              | ScanOnly
              | NoScan
                deriving Eq

data Option = OStore String
            | OPort String
            | OLogDir String
            | OHost String
            | OContentDir String
            | OStaticDir String
            | OScan ScanType
            | OHelp
              deriving Eq

data Err a = Err String | Val a
instance Monad Err where
    return = Val
    fail = Err
    (Err s) >>= _ = Err s
    (Val x) >>= f = f x

errs :: [Err a] -> ([String], [a])
errs (x:xs) = let (es, as) = errs xs
              in case x of
                   Err e -> (e:es, as)
                   Val x' -> (es, x':as)
errs [] = ([], [])

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

parseOptions :: [String] -> Either [String] (Maybe Config)
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
