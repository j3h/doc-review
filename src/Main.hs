{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad      ( when )
import           Data.Foldable      ( forM_ )
import           Network.URI        ( parseRelativeReference )
import           Prelude hiding     ( mapM_ )
import           System.Environment ( getArgs )
import           System.Exit        ( exitFailure, exitSuccess )

import           DocReview.App      ( runServer )
import           Config             ( parseArgs, unUsage, Usage, Action(..) )
import           Analyze            ( analyze )
import           State.Types        ( State, ChapterId, CommentId, State
                                    , addChapter
                                    )
import           DocReview.Scan     ( showAnalysis )
import qualified Report              as Report
import qualified Config.Command.Run  as Run
import qualified Config.Command.Scan as Scan
import qualified State.Logger        as L

showUsage :: Usage -> IO ()
showUsage = putStr . unUsage 78

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left usg ->
        do showUsage usg
           exitFailure

    Right (Help usg) ->
        do showUsage usg
           exitSuccess

    Right (Report cfg) ->
        putStr . Report.genReport cfg =<< Report.analyzeFiles cfg

    Right (RunServer cfg) ->
        do st <- maybe return L.wrap (Run.cfgLogTo cfg) =<< Run.cfgStore cfg

           -- Scan the content directory (unless requested not to)
           when (Run.cfgScanOnStart cfg) $
                do chapters <- analyze $ Run.cfgContentDir cfg
                   storeChapters chapters st

           runServer cfg st

    Right (Scan cfg) ->
        do chapters <- analyze $ Scan.contentDir cfg
           case Scan.store cfg of
             -- If a store was specified, scan the directory and store
             -- the results in the store
             Just mk -> storeChapters chapters =<< mk

             -- If no store was specified, scan the directory and dump
             -- an analysis of the scan results to the console (check
             -- for duplicate comment ids)
             Nothing -> putStr $ unlines $ showAnalysis chapters

storeChapters :: [(String, [(Maybe ChapterId, [CommentId])])]
              -> State -> IO ()
storeChapters files st = do
  forM_ files $ \(fn, chapters) -> do
      let uri = parseRelativeReference fn
      forM_ chapters $ \(mChId, cIds) ->
          maybe (return ()) (\chId -> addChapter st chId cIds uri) mChId
