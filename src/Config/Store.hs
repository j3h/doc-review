module Config.Store
    ( storeOptDescr
    , ParseStore
    , psConst
    , psArg
    , psArgStr
    , storeDocumentation
    )
where

import System.Console.GetOpt
import Control.Monad ( msum, guard )
import Config.Format ( twoColumn, indent, wrap )

storeOptDescr :: Monad m => [ParseStore b] -> (b -> a -> a)
              -> OptDescr (m (a -> a))
storeOptDescr stores set
    = Option "s" ["store"] (ReqArg f "STORE") "The state storage type."
    where f sType =
              case parseStore stores sType of
                Nothing -> fail $ "Unknown store type: " ++ show sType
                Just st -> return $ set st

parseStore :: [ParseStore a] -> String -> Maybe a
parseStore stores sType = msum $ map (`psParse` sType) stores

data ParseStore a = PS { psParse :: String -> Maybe a
                       , psKey   :: String
                       , psDescr :: String
                       }

psConst :: String -> a -> String -> ParseStore a
psConst k v = PS f k
    where
      f sType = guard (sType == k) >> return v

psArg :: String -> String -> (String -> Maybe a) -> String -> ParseStore a
psArg pfx argName p = PS f (pfx ++ ':':argName)
    where
      f sType = case break (== ':') sType of
                  (k, (':':rest)) | k == pfx -> p rest
                  _                          -> Nothing

psArgStr :: String -> String -> (String -> a) -> String -> ParseStore a
psArgStr pfx argName f = psArg pfx argName (Just . f)

storeDocumentation :: [ParseStore a] -> Int -> [String]
storeDocumentation stores lineLength =
    wrap lineLength "Specifying data storage:" ++ showStores
    where
      showStores = indent 2 $
                   twoColumn (lineLength - 2) $
                   map showStore stores
      showStore ps = (psKey ps, psDescr ps)
