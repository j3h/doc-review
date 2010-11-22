module Config.Format
    ( twoColumn
    , wrap
    , findBreak
    , indent
    )
where

import Control.Monad ( mplus )
import Data.Char ( isAlphaNum, isSpace )
import Data.Maybe ( fromMaybe )

--------------------------------------------------
-- Laying out documentation

-- XXX: This should be generalized so that you can pass it a list of
-- rows of arbitrary numbers of columns and get a table layout.

-- |Lay out pairs of strings in two columns, under the specified line length
twoColumn :: Int -> [(String, String)] -> [String]
twoColumn w ps = concatMap fmt1 ps
    where
      firstColN = min (maximum $ map (length . fst) ps) 40

      secondColN = w - firstColN - 2

      fmt1 (a, b) = hcat firstColN secondColN a b

      hcat n1 n2 = go
          where
            go [] [] = []
            go xs ys =
                let (c1, xs') = findBreak n1 xs
                    (c2, ys') = findBreak n2 ys
                    l = fillCol n1 c1 ++ "  " ++ fillCol n2 c2
                in l : go xs' ys'
      fillCol n xs = take n $ xs ++ repeat ' '

-- |Wrap a string at a certain line length, attempting to break at
-- word boundaries
wrap :: Int -> String -> [String]
wrap n = go
    where
      go s = case findBreak n s of
               ([], []) -> []
               (l,  s') -> l:go s'

-- |Attempt to break a string at a word boundary. As a last resort,
-- arbitrarily break at the specified line limit
findBreak :: Int -> String -> (String, String)
findBreak n s = let (l, s') = fromMaybe (splitAt n s) $ go 0 id s
                in (l, dropWhile isSpace s')
    where
      -- Look for the last word boundary in the first n characters of
      -- the String, and if there is a word boundary, return the
      -- String up to that last boundary and the remainder of the
      -- String
      go :: Int    -- ^Which character we are examining now
         -> ShowS  -- ^The String up to this point
         -> String -- ^The remainder of the String that we have to examine
         -> Maybe (String, String)
            -- ^If we found a breaking point before getting to the
            -- limit, this is the String up to that breaking point and
            -- then the remainder
      go l acc xs =
          case xs of
            -- If we get to the end and we haven't violated the length
            -- constraint, then return what we've got.
            []                    -> ok
            (c:cs)
                -- This violates the length constraint, so fail.
                | l >= n       -> Nothing

                -- An alphanumeric character is not allowed to be a
                -- break point, so try to find a breaking point later
                -- on or fail.
                | isAlphaNum c -> next c cs

                -- This is a valid breaking point, so try to see if
                -- there is one further on in the string and if there
                -- is not, then this one is the longest.
                | otherwise    -> next c cs `mplus` ok
          where
            -- acc [] is what we have processed before examining this
            -- character
            ok = return (acc [], xs)

            -- next looks for a breaking point further on in the
            -- String
            next c cs = go (l + 1) (acc . (c:)) cs

-- |Prepend the specified number of spaces to each String
indent :: Int -> [String] -> [String]
indent n = map $ showString $ replicate n ' '
