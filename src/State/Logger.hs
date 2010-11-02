{-|
  Module that logs all state actions to a file so that they could
  theoretically be reconstructed.
 -}
module State.Logger
    ( wrap
    )
where

import System.IO ( stderr, openBinaryFile, hPutStrLn, hClose
                 , IOMode(AppendMode), Handle )
import Data.ByteString.Lazy as B
import Data.Binary ( Binary(..), getWord8, putWord8, encode )
import Control.Applicative ( (<$>), (<*>) )
import Data.Word ( Word8 )
import Control.Concurrent.MVar ( MVar, newMVar, modifyMVar )

import State.Types

data Action = AddComment CommentId (Maybe ChapterId) Comment
            | AddChapter ChapterId [CommentId]
              deriving (Eq, Show)

instance Binary Action where
    get = do
      ty <- getWord8
      case ty of
        0 -> AddComment <$> get <*> get <*> get
        1 -> AddChapter <$> get <*> get
        _ -> error $ "Bad type code: " ++ show ty

    put (AddComment cId mChId c) =
        do put (0::Word8)
           put cId
           put mChId
           put c
    put (AddChapter chId cs) =
        do put (1::Word8)
           put chId
           put cs

-- |Wrap a store so that all of its modifying operations are logged to
-- a file, so that the data could be restored from
-- the log
wrap :: FilePath -> State -> IO State
wrap logFileName st = do
  ref <- newMVar Nothing
  return st { addComment = \cId mChId c -> do
                             writeLog logFileName ref $ AddComment cId mChId c
                             addComment st cId mChId c
            , addChapter = \chId cs -> do
                             writeLog logFileName ref $ AddChapter chId cs
                             addChapter st chId cs
            }

writeLog :: FilePath -> MVar (Maybe Handle) -> Action -> IO ()
writeLog fn v act =
    modifyMVar v $ \mh -> do
      -- Try to get a handle
      mh' <- case mh of
               Nothing -> tryOpenAppend
               Just h  -> return $ Just h

      -- If we got a handle, try to log to it
      res <- maybe (return Nothing) gotHandle mh
      return (res, ())

    where
      -- If we don't have a handle, try to open one
      tryOpenAppend =
          (Just `fmap` openBinaryFile fn AppendMode)
          `catch` \e ->
              do hPutStrLn stderr $ "Failed to open log file: " ++ show e
                 return Nothing

      -- Try to write the log entry, but don't raise an exception if
      -- we can't, so that we have the best chance of actually saving
      -- the data
      gotHandle h = writeLogEntry h `catch` cleanUp h

      -- Write the data to the handle, and return the good handle for
      -- use next time
      writeLogEntry h = do
        B.hPut h $ encode act
        return (Just h)

      -- Close the handle, print a message recording the exception,
      -- and store Nothing for next time.
      cleanUp h e = do
        hClose h `catch` \_ -> return ()
        hPutStrLn stderr $ "Failed to write log entry: " ++ show e
        return Nothing
