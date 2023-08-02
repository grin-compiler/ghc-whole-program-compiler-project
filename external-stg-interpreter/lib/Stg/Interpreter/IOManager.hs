{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Stg.Interpreter.IOManager where

import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Time.Clock

import Data.Monoid ((<>))
import Foreign.C.Types
import qualified Language.C.Inline as C
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

import Stg.Interpreter.Base

-------- I/O manager
C.context (C.baseCtx <> C.vecCtx)

C.include "<sys/select.h>"
C.include "<errno.h>"
C.include "<stdlib.h>"

fdSetSize :: Int
fdSetSize = fromIntegral [C.pure| int { FD_SETSIZE } |]

waitForFDs :: V.Vector CInt -> V.Vector CInt -> CInt -> IO (CInt, CInt)
waitForFDs readFDV writeFDV maxfd = do
  C.withPtr $ \selectResult -> do
    [C.block| int {
      struct timeval tv = {.tv_sec = 0, .tv_usec = 1}; // TODO
      fd_set rfd, wfd;
      int fd;

      FD_ZERO(&rfd);
      FD_ZERO(&wfd);

      for (int i = 0; i < $vec-len:readFDV; i++) {
        fd = $vec-ptr:(int *readFDV)[i];
        FD_SET(fd, &rfd);
      }

      for (int i = 0; i < $vec-len:writeFDV; i++) {
        fd = $vec-ptr:(int *writeFDV)[i];
        FD_SET(fd, &wfd);
      }

      int numFound;

      while ((numFound = select( $(int maxfd) + 1, &rfd, &wfd, NULL, &tv)) < 0) {
        if (errno != EINTR) break;
      }

      $(int* selectResult)[0] = numFound;
      return errno;
    } |]

{-
enum FdState {
    RTS_FD_IS_READY     = 0,
    RTS_FD_IS_BLOCKING  = 1,
    RTS_FD_IS_INVALID   = 2,
    RTS_SELECT_FAILURE  = 3,
};
-}

fdPollReadState :: CInt -> IO CInt
fdPollReadState fd = do
  [C.block| int {
    int r;
    fd_set rfd;
    struct timeval now;

    FD_ZERO(&rfd);
    FD_SET($(int fd), &rfd);

    /* only poll */
    now.tv_sec  = 0;
    now.tv_usec = 0;
    for (;;)
    {
        r = select( $(int fd) + 1, &rfd, NULL, NULL, &now);
        /* the descriptor is sane */
        if (r != -1)
            break;

        switch (errno)
        {
            case EBADF: return 2; //RTS_FD_IS_INVALID
            case EINTR: continue;
            default:    return 3; //RTS_SELECT_FAILURE
        }
    }

    if (r == 0)
        return 1; //RTS_FD_IS_BLOCKING
    else
        return 0; //RTS_FD_IS_READY
  } |]

fdPollWriteState :: CInt -> IO CInt
fdPollWriteState fd = do
  [C.block| int {
    int r;
    fd_set wfd;
    struct timeval now;

    FD_ZERO(&wfd);
    FD_SET($(int fd), &wfd);

    /* only poll */
    now.tv_sec  = 0;
    now.tv_usec = 0;
    for (;;)
    {
        r = select( $(int fd) + 1, NULL, &wfd, NULL, &now);
        /* the descriptor is sane */
        if (r != -1)
            break;

        switch (errno)
        {
            case EBADF: return 2; //RTS_FD_IS_INVALID
            case EINTR: continue;
            default:    return 3; //RTS_SELECT_FAILURE
        }
    }

    if (r == 0)
        return 1; //RTS_FD_IS_BLOCKING
    else
        return 0; //RTS_FD_IS_READY
  } |]

{-
  TODO:
    done - collect min delay
    done - collect read fd handles
    done - collect write fd handles
    - raise exceptions for invalid fds
    done - query fds with select
    - raise exceptions for invalid fds
    done - enable threads
-}

handleBlockedDelayWait :: M ()
handleBlockedDelayWait = do
  tsList <- gets $ IntMap.toList . fmap tsStatus . ssThreads
  now <- liftIO getCurrentTime
  let maxSeconds  = 31 * 24 * 60 * 60 -- some OS have this constraint
      maxDelay    = secondsToNominalDiffTime maxSeconds
      delaysT     = [(tid, t `diffUTCTime` now) | (tid, ThreadBlocked (BlockedOnDelay t)) <- tsList]
      minDelay    = max 0 $ minimum $ maxDelay : delays
      readFDsT    = [(tid, fromIntegral fd :: CInt) | (tid, ThreadBlocked (BlockedOnRead fd)) <- tsList]
      writeFDsT   = [(tid, fromIntegral fd :: CInt) | (tid, ThreadBlocked (BlockedOnWrite fd)) <- tsList]
      delays      = map snd delaysT
      readFDs     = map snd readFDsT
      writeFDs    = map snd writeFDsT
      fdList      = readFDs ++ writeFDs
      maxFD       = maximum fdList
  -- TODO: detect deadlocks
  unless (null fdList) $ do
    -- query file descriptors
    (selectResult, errorNo) <- liftIO $ waitForFDs (V.fromList readFDs) (V.fromList writeFDs) maxFD
    when (selectResult < 0) $ error $ "select error, errno: " ++ show errorNo

    forM_ readFDsT $ \(tid, fd) -> do
      liftIO (fdPollReadState fd) >>= \case
        0 -> do
          ts <- getThreadState tid
          updateThreadState tid ts {tsStatus = ThreadRunning}
        _ -> pure () -- TODO

    forM_ writeFDsT $ \(tid, fd) -> do
      liftIO (fdPollWriteState fd) >>= \case
        0 -> do
          ts <- getThreadState tid
          updateThreadState tid ts {tsStatus = ThreadRunning}
        _ -> pure () -- TODO
