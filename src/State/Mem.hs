module State.Mem
    ( new
    )
where

import State.Types

import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Foldable ( toList )
import Control.Concurrent.MVar ( newMVar, withMVar, modifyMVar_ )
import Control.Arrow ( second )

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
             }

--------------------------------------------------
-- Pure

data MemState = MemState { chs :: Map.Map ChapterId (Set.Set CommentId)
                         , cms :: Map.Map CommentId (S.Seq Comment)
                         }

emptyMemState :: MemState
emptyMemState = MemState Map.empty Map.empty

getCounts' :: Maybe ChapterId -> MemState -> [(CommentId, Int)]
getCounts' chId st =
    map (second S.length) $ Map.toList $
    case chId >>= (`Map.lookup` chs st) of
      Nothing   -> cms st
      Just cIds ->
          let p k _ =  k `Set.member` cIds
          in Map.filterWithKey p (cms st)

addComment' :: CommentId -> Maybe ChapterId -> Comment -> MemState -> MemState
addComment' cId chId c st =
    st { chs = case chId of
                 Nothing   -> chs st
                 Just chap -> Map.alter (addSet cId) chap (chs st)
       , cms = Map.alter (addSeq c) cId $ cms st
       }
    where
      addSet x = return . (Set.insert x) . maybe Set.empty id
      addSeq x = return . (S.|> x) . maybe S.empty id
