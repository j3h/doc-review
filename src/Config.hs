{-| Configuration and command-line arguments -}
module Config
    ( Action(..)
    , parseArgs
    , usage
    , Usage
    , unUsage
    )
where

import Data.Monoid ( Monoid(..) )

import Config.Store ( ParseStore, psConst, psArgStr
                    , storeDocumentation )
import Config.GetOpt ( optSpec, noArgs )
import Config.Types ( Usage(..), strUsage, ActionSpec(..)
                    , parseArgsG, CommandResult(..) )
import Config.Format ( twoColumn, indent )
import qualified Config.Command.Scan as Scan
import qualified Config.Command.Report as Report
import qualified Report as Report
import qualified Config.Command.Run as Run
import State.Types ( State )
import qualified State.Mem ( new )
import qualified State.Disk ( new )
import qualified State.SQLite ( new )

data Action = Help Usage
            | RunServer Run.Config
            | Report Report.ReportConfig
            | Scan Scan.Config

parseArgs :: [String] -> Either Usage Action
parseArgs args = case parseArgsG commands args of
                   BadCommand u    -> Left $ usage `mappend` u
                   HelpRequest u   -> Right $ Help u
                   CommandFailed u -> Left u
                   CommandOk x     -> Right $ x

-- |Add the preamble to the usage information and turn it into a String
unUsage :: Int -> Usage -> String
unUsage n u = let (Usage f) = strUsage preamble `mappend` u in unlines $ f n
    where
      preamble =
          "Web-based document review system, based on the system used for \
          \Real World Haskell <http://book.realworldhaskell.org/>"

usage :: Usage
usage =
    mconcat
    [ Usage cmds
    , strUsage "Use <command> --help for help on any individual command."
    , Usage $ storeDocumentation stores
    ]
    where
      cmds n = "Commands:":indent 2 (twoColumn (n - 2) $ map showCmd $ commands)
      showCmd as = (asName as, asDescr as)

commands :: [ActionSpec Action]
commands =
    [ optSpec "run" RunServer (Run.opts stores) Run.mkCfg
      "Run the HTTP server for Web-based document review, that can serve \
      \the static content, the AJAX API for adding and displaying comments, \
      \and the Atom feed and HTML comment viewing API"
    , optSpec "scan" Scan (Scan.opts stores) Scan.mkCfg
      "Scan a set of documents for commentable items and (optionally) update \
      \a store"
    , optSpec "report" Report Report.opts Report.mkCfg
      "Process a set of binary log files and produce a usage report"
    , optSpec "help" Help [] (noArgs usage)
      "Show this help text"
    ]

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
