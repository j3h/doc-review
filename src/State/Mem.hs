module State.Mem
    ( new
    )
where

import State.Types

import Data.Maybe ( listToMaybe, fromMaybe )
import Data.Function ( on )
import Data.List ( sortBy )
import Control.Monad ( mplus )
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Foldable ( toList )
import Control.Concurrent.MVar ( newMVar, withMVar, modifyMVar_ )
import Control.Arrow ( second, (***) )
import Network.URI ( URI )

--------------------------------------------------
-- Stateful

new :: IO State
new = do
  v <- newMVar emptyMemState
  return $ State
             { findComments = \cId        ->
                              withMVar v $
                              return . maybe [] toList . Map.lookup cId . cms

             , getCounts    = \chId       ->
                              withMVar v $ return . getCounts' chId

             , addComment   = \cId chId c ->
                              modifyMVar_ v $ return . addComment' cId chId c

             , addChapter   = \chId cIds mURI ->
                              modifyMVar_ v $ return . addChapter' chId cIds mURI

             , getLastInfo  = \sid ->
                              withMVar v $ return . getInfo' sid

             , getChapterComments =
               \chId -> withMVar v $ return . getChapterComments' chId

             , getChapterURI = \chId -> withMVar v $ return . chapterURI chId
             }

--------------------------------------------------
-- Pure

data MemState = MemState { chs :: Map.Map ChapterId (Maybe URI, Set.Set CommentId)
                         , cms :: Map.Map CommentId (S.Seq Comment)
                         }

chapterComments :: ChapterId -> MemState -> Set.Set CommentId
chapterComments chId = maybe Set.empty snd . Map.lookup chId . chs

chapterURI :: ChapterId -> MemState -> Maybe URI
chapterURI chId st = fst =<< Map.lookup chId (chs st)

withChapterComments :: (Set.Set CommentId -> Set.Set CommentId)
                    -> ChapterId -> MemState -> MemState
withChapterComments = withChapter . second

withChapter :: ((Maybe URI, Set.Set CommentId) -> (Maybe URI, Set.Set CommentId))
            -> ChapterId -> MemState -> MemState
withChapter f chId st =
    let upd x = case f $ fromMaybe (Nothing, Set.empty) x of
                  (Nothing, s) | Set.null s -> Nothing
                  p                         -> Just p
    in st { chs = Map.alter upd chId $ chs st }

emptyMemState :: MemState
emptyMemState = MemState Map.empty Map.empty

getInfo' :: SessionId -> MemState -> Maybe SessionInfo
getInfo' sId =
    fmap toInfo .
    listToMaybe .
    take 1 .
    reverse .
    sortBy (compare `on` cDate) .
    filter ((== sId) . cSession) .
    concatMap toList .
    Map.elems .
    cms
    where
      toInfo c = SessionInfo (cName c) (cEmail c)

getCounts' :: Maybe ChapterId -> MemState -> [(CommentId, Int)]
getCounts' mChId st =
    map (second S.length) $ Map.toList $
    case mChId of
      Nothing   -> cms st
      Just chId ->
          let cIds = chapterComments chId st
              p k _ =  k `Set.member` cIds
          in Map.filterWithKey p (cms st)

addChapter' :: ChapterId -> [CommentId] -> Maybe URI -> MemState -> MemState
addChapter' chId cIds mURI =
    withChapter ((mURI `mplus`) *** (Set.union $ Set.fromList cIds)) chId

addComment' :: CommentId -> Maybe ChapterId -> Comment -> MemState -> MemState
addComment' cId mChId c st =
    maybe id (withChapterComments (Set.insert cId)) mChId $
    st { cms = Map.alter (addSeq c) cId $ cms st }
    where
      addSeq x = return . (S.|> x) . maybe S.empty id

getChapterComments' :: ChapterId -> MemState -> [(CommentId, Comment)]
getChapterComments' chId st =
    let sortDateDesc       = sortBy $ flip (compare `on` (cDate . snd))
        lookupComments cId = maybe [] (map ((,) cId) . toList) $
                             Map.lookup cId $ cms st
    in sortDateDesc $ concatMap lookupComments $
       toList $ chapterComments chId st
