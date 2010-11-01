module Privilege
    ( tryDropPrivilege
    )
where

import Control.Applicative ( (<$>) )
import Data.IORef ( newIORef, readIORef , writeIORef )
import System.Posix.User ( getUserEntryForName, setUserID
                         , userID, getRealUserID )
import Control.Monad ( join, unless )

-- Obtain an action that will attempt to drop privileges if the
-- environment and configuration allow it. Ideally, we would be able
-- to drop privileges after binding to the port and before doing
-- anything else. This is a compromise that provides a modicum of
-- safety if the request can co-opt the server, but no protection from
-- any local attacks. In particular, all of the server start-up stuff
-- happened before this, including creating the database. This could
-- potentially cause permission errors.
tryDropPrivilege :: String -> IO (IO ())
tryDropPrivilege username = do
  -- Figure out what user id we need to switch to (if any)
  uid <- getRealUserID
  targetUID <-
      do t <- userID <$> getUserEntryForName username
         case uid of
           -- Root:
           0            ->
               return $ Just t

           -- We already are the target user:
           _ | t == uid ->
                 return Nothing

           -- Anyone else:
           _            ->
               error $ "Only root can change users \
                       \(trying to run as " ++ username ++ ")"

  case targetUID of
    Nothing     ->
        -- If we have no target UID, then just return a no-op
        return $ return ()

    Just target ->
        -- Build and return an action that will ensure that we are
        -- running as the target user before proceeding
        do
          -- The variable that holds the action that ensures that we
          -- have dropped privileges
          startRef <- newIORef $ return ()

          -- Put the privilege dropping action in
          writeIORef startRef $
            do putStrLn $
                 "Dropping privileges: switching to user " ++ show target

               setUserID target `catch` \e ->
                   -- Check that we didn't lose a race
                   -- trying to drop privileges. If we lost,
                   -- then everything's OK because the
                   -- privileges are already dropped.
                   do newUID <- getRealUserID

                      -- If it wasn't losing the race,
                      -- raise it again here
                      unless (newUID == target) $ ioError e

               finalUID <- getRealUserID
               if finalUID == target
                 -- replace the action with a no-op
                 then writeIORef startRef $ return ()
                 else error "Failed to drop privileges"

          return $ join $ readIORef startRef
