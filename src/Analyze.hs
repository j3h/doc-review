module Analyze
    ( htmlIds
    , analyzeFile
    , analyze
    )
where
import qualified Text.HTML.Tagchup.Parser as T ( runSoup )
import qualified Text.HTML.Tagchup.Tag.Match as M
import qualified Text.HTML.Tagchup.Tag as T
import qualified Text.XML.Basic.Name.LowerCase as N
import qualified Text.XML.Basic.Attribute as A
import Data.Maybe ( mapMaybe, listToMaybe )
import Control.Arrow ( second, first )
import Control.Monad ( forM )
import System.Directory ( getDirectoryContents )
import System.FilePath ( (</>), takeExtension )
import qualified Data.Text as Txt
import State.Types ( ChapterId, mkChapterId, CommentId, mkCommentId )

analyze :: FilePath -> IO [(FilePath, [(Maybe ChapterId, [CommentId])])]
analyze fn = do
  let ignoreEntry = (`elem` [".", ".."])

      processEntry n
          | takeExtension n == ".html" = processFile n `catch` \_ ->
                                         processDir n
          | otherwise                  = processDir n

      processFile n = (\xs -> [(n, xs)]) `fmap` analyzeFile (fn </> n)
      processDir n = map (first (n </>)) `fmap` analyze (fn </> n)

  fs <- filter (not . ignoreEntry) `fmap` getDirectoryContents fn

  results <- forM fs $ \n ->
             processEntry n `catch` \_ -> return []

  return $ concat results

analyzeFile :: FilePath -> IO [(Maybe ChapterId, [CommentId])]
analyzeFile = fmap htmlIds . readFile

-- Extract the paragraph ids from the chapter divs
htmlIds :: String -> [(Maybe ChapterId, [CommentId])]
htmlIds = map (first (mkChapterId . Txt.pack =<<)) .
          map (second (mapMaybe (mkCommentId . Txt.pack) . findIds)) .
          findChapterContent .
          T.runSoup

findIds :: [T.T N.T String] -> [String]
findIds = concatMap getId . concatMap snd . mapMaybe T.maybeOpen

getId :: A.T N.T String -> [String]
getId (A.Cons (A.Name (N.Cons "id")) i) = [i]
getId _                        = []

findChapterContent :: [T.T N.T String] -> [(Maybe String, [T.T N.T String])]
findChapterContent ts =
    case dropWhile (not . divChapter) ts of
      (t:ts') -> let chId = listToMaybe (chapId t)
                 in case findClose "div" ts' of
                      Nothing ->
                          -- No close tag, so no content, but check
                          -- for well-formed tags further on
                          (chId, []):findChapterContent ts'
                      Just (f, ts'') ->
                          -- Well-formed tag, so take its content
                          (chId, f []):findChapterContent ts''
      _ -> []
    where
      chapId (T.Open _ attrs) = concatMap getId attrs
      chapId _ = []

divChapter :: T.T N.T String -> Bool
divChapter (T.Open (T.Name (N.Cons "div")) attrs) = any hasChapterClass attrs
divChapter _ = False

hasChapterClass :: A.T N.T String -> Bool
hasChapterClass (A.Cons (A.Name (N.Cons "class")) s) = "chapter" `elem` words s
hasChapterClass _ = False

findClose :: String -> [T.T N.T String]
          -> Maybe ([T.T N.T String] -> [T.T N.T String], [T.T N.T String])
findClose _ [] = Nothing
findClose n (t:ts)
    | M.openNameLit n t =
        do -- Find the close of this inner tag
           (inside, ts') <- findClose n ts
           -- Resume finiding the close of the outer tag
           (inside', ts'') <- findClose n ts'
           -- Put the content of the inner tag
           let inner = (t:) . inside . (T.Close (T.Name (N.Cons n)):)
           return $ (inner . inside', ts'')
    | M.closeLit n t    = Just (id, ts)
    | otherwise         =
        do (inside, ts') <- findClose n ts
           return ((t:) . inside, ts')
