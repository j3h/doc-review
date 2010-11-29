module Config.Command.Report
    ( mkCfg
    , opts
    )
where

import Config.GetOpt ( MkCfg, Opts )
import Report ( ReportConfig(..) )

mkCfg :: MkCfg ReportConfig
mkCfg = return . ReportConfig

opts :: Opts ReportConfig
opts = []