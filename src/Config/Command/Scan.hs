module Config.Command.Scan
    ( Config(..)
    , opts
    , mkCfg
    )
where

import Config.Store ( storeOptDescr, ParseStore )
import Config.GetOpt ( Opts, MkCfg, noArgs )
import System.Console.GetOpt
import State.Types ( State )

data Config =
    Config
    { contentDir :: !(FilePath)
    , store      :: !(Maybe (IO State))
    -- ^If this is Nothing, then just print out what we find.
    }

opts :: [ParseStore (IO State)] -> Opts Config
opts ss =
    [ Option "" ["content-dir"] (ReqArg setCDir "DIR")
      "Scan this directory for content (default: \"content\")"
    , storeOptDescr ss $ \st cfg -> cfg { store = Just st }
    ]
    where
      setCDir p = return $ \cfg -> cfg { contentDir = p }

mkCfg :: MkCfg Config
mkCfg = noArgs $ Config "content" Nothing
