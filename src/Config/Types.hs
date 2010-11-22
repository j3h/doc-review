module Config.Types
    ( Usage(..)
    , preUsage
    , strUsage
    , ActionSpec(..)
    , isHelpRequest
    , parseArgsG
    , CommandResult(..)
    )
where

import Data.Monoid ( Monoid(..) )
import Data.Maybe ( fromMaybe )
import Control.Monad ( msum )
import Config.Format ( wrap )

newtype Usage = Usage (Int -> [String])

instance Monoid Usage where
    mempty = Usage $ const []
    mappend (Usage f1) (Usage f2) = Usage $ \n -> f1 n ++ ([]:f2 n)

strUsage :: String -> Usage
strUsage = Usage . flip wrap

preUsage :: [String] -> Usage
preUsage = Usage . const

data ActionSpec a
    = ActionSpec { asName  :: String
                 , asDescr :: String
                 , asParse :: [String] -> Maybe (CommandResult a)
                 }

data CommandResult a = BadCommand Usage
                     | HelpRequest Usage
                     | CommandFailed Usage
                     | CommandOk a

parseArgsG :: [ActionSpec a] -> [String] -> CommandResult a
parseArgsG _ [] = BadCommand $ strUsage "Please specify a command"
parseArgsG acts args =
    fromMaybe (BadCommand unknown) $ msum $ map (`asParse` args) acts
    where
      unknown = strUsage ("Unknown command: " ++ show args)

isHelpRequest :: [String] -> Bool
isHelpRequest args = any (`elem` args) ["-h", "--help"]
