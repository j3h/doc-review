module Config.GetOpt
    ( Opts
    , MkCfg
    , optSpec
    , noArgs
    , Err
    )
where

import Data.Monoid ( mappend )
import Control.Monad ( liftM, ap, guard )
import Config.Types ( ActionSpec(..), preUsage, CommandResult(..)
                    , isHelpRequest )
import Config.Format ( wrap, indent )
import Data.Char ( isSpace )
import System.Console.GetOpt

type Opts cfg = [OptDescr (Err (cfg -> cfg))]
type MkCfg cfg = [String] -> Err cfg

optSpec :: String -> (cfg -> a) -> Opts cfg -> MkCfg cfg -> String
        -> ActionSpec a
optSpec name mkAction osOpts osCfg desc =
    ActionSpec
    { asName = name
    , asDescr = desc
    , asParse = parseOptSpec
    }
    where
      actUsage = preUsage [usageInfo (unlines $ wrap 78 $ desc) osOpts]

      strip = reverse . dropWhile isSpace . reverse

      parseOptSpec args =
          let (os, args', es) = getOpt Permute osOpts args
              flgs = if null es
                     then sequenceE os
                     else Err $ map strip es ++ toErrors (sequenceE os)
              result xs = case (flgs, osCfg xs) of
                            (Err es1, Err es2) -> Err $ es1 ++ es2
                            (_, cfg)           ->
                                foldr ($) `liftM` cfg `ap` flgs
              handle (Err msgs)  =
                  CommandFailed $ actUsage `mappend` preUsage
                                    ("Errors parsing arguments:":indent 2 msgs)
              handle (Val cfg) =
                  CommandOk $ mkAction cfg
              accepted actArgs =
                  if isHelpRequest actArgs
                  then HelpRequest actUsage
                  else handle $ result actArgs
          in case args' of
               (k:actArgs) -> guard (k == name) >> return (accepted actArgs)
               _                       -> Nothing


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
