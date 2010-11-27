module Config.Command.Scan
    ( Config(..)
    , opts
    , mkCfg
    )
where

import Config.Store ( storeOptDescr, ParseStore )
import Config.GetOpt ( Opts, MkCfg, noArgs, contentDirDesc )
import State.Types ( State )

data Config =
    Config
    { contentDir :: !(FilePath)
    , store      :: !(Maybe (IO State))
    -- ^If this is Nothing, then just print out what we find.
    }

opts :: [ParseStore (IO State)] -> Opts Config
opts ss =
    [ contentDirDesc $ \p -> return $ \cfg -> cfg { contentDir = p }
    , storeOptDescr ss $ \st cfg -> cfg { store = Just st }
    ]

mkCfg :: MkCfg Config
mkCfg = noArgs $ Config "." Nothing
