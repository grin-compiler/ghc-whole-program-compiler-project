{-# LANGUAGE LambdaCase #-}
module Stg.Interpreter.State.Thread where

import Data.Time.Clock
import Data.ByteString.Char8 (ByteString)
import Stg.Interpreter.State.Allocator

-- threading

data AsyncExceptionMask
  = NotBlocked
  | Blocked     {isInterruptible :: !Bool}
  deriving (Eq, Ord, Show)

data ThreadState
  = ThreadState
  { tsCurrentResult     :: [AtomAddr] -- Q: do we need this? A: yes, i.e. MVar read primops can write this after unblocking the thread
  , tsStackTop          :: Maybe StackAddr
  , tsStatus            :: !ThreadStatus
  , tsBlockedExceptions :: [Int] -- ids of the threads waitng to send an async exception
  , tsBlockExceptions   :: !Bool  -- block async exceptions
  , tsInterruptible     :: !Bool  -- interruptible blocking of async exception
--  , tsAsyncExMask     :: !AsyncExceptionMask
  , tsBound             :: !Bool
  , tsLocked            :: !Bool  -- Q: what is this for? is this necessary?
  , tsCapability        :: !Int   -- NOTE: the thread is running on this capability ; Q: is this necessary?
  , tsLabel             :: !(Maybe ByteString)
  }
  deriving (Eq, Ord, Show)


--------------

-- NOTE: the BlockReason data type is some kind of reification of the blocked operation
data BlockReason
  = BlockedOnMVar         Int (Maybe AtomAddr) -- mvar id, the value that need to put to mvar in case of blocking putMVar#, in case of takeMVar this is Nothing
  | BlockedOnMVarRead     Int       -- mvar id
  | BlockedOnBlackHole
  | BlockedOnThrowAsyncEx Int AtomAddr  -- target thread id, exception
  | BlockedOnSTM
  | BlockedOnForeignCall            -- RTS name: BlockedOnCCall
  | BlockedOnRead         Int       -- file descriptor
  | BlockedOnWrite        Int       -- file descriptor
  | BlockedOnDelay        UTCTime   -- target time to wake up thread
  deriving (Eq, Ord, Show)

data ThreadStatus
  = ThreadRunning
  | ThreadBlocked   BlockReason
  | ThreadFinished  -- RTS name: ThreadComplete
  | ThreadDied      -- RTS name: ThreadKilled
  deriving (Eq, Ord, Show)

isThreadLive :: ThreadStatus -> Bool
isThreadLive = \case
  ThreadFinished  -> False
  ThreadDied      -> False
  _ -> True

{-
threadStatus :: ThreadId -> IO ThreadStatus
threadStatus (ThreadId t) = IO $ \s ->
   case threadStatus# t s of
    (# s', stat, _cap, _locked #) -> (# s', mk_stat (I# stat) #)
   where
        -- NB. keep these in sync with includes/rts/Constants.h
     mk_stat 0  = ThreadRunning
     mk_stat 1  = ThreadBlocked BlockedOnMVar
     mk_stat 2  = ThreadBlocked BlockedOnBlackHole
     mk_stat 6  = ThreadBlocked BlockedOnSTM
     mk_stat 10 = ThreadBlocked BlockedOnForeignCall
     mk_stat 11 = ThreadBlocked BlockedOnForeignCall
     mk_stat 12 = ThreadBlocked BlockedOnException
     mk_stat 14 = ThreadBlocked BlockedOnMVar -- possibly: BlockedOnMVarRead
     -- NB. these are hardcoded in rts/PrimOps.cmm
     mk_stat 16 = ThreadFinished
     mk_stat 17 = ThreadDied
     mk_stat _  = ThreadBlocked BlockedOnOther
-}

{-
data BlockedStatus
  = NotBlocked
  | BlockedOnMVar
  | BlockedOnMVarRead
  | BlockedOnBlackHole
  | BlockedOnRead
  | BlockedOnWrite
  | BlockedOnDelay
  | BlockedOnSTM
  -- Win32 only
  | BlockedOnDoProc
  -- Only relevant for THREADED_RTS
  | BlockedOnCCall
  | BlockedOnCCall_Interruptible

  -- Involved in a message sent to tso->msg_cap
  | BlockedOnMsgThrowTo
  | ThreadMigrating
-}

{-
#define NotBlocked          0
#define BlockedOnMVar       1
#define BlockedOnMVarRead   14 /* TODO: renumber me, see #9003 */
#define BlockedOnBlackHole  2
#define BlockedOnRead       3
#define BlockedOnWrite      4
#define BlockedOnDelay      5
#define BlockedOnSTM        6

/* Win32 only: */
#define BlockedOnDoProc     7

/* Only relevant for THREADED_RTS: */
#define BlockedOnCCall      10
#define BlockedOnCCall_Interruptible 11
   /* same as above but permit killing the worker thread */

/* Involved in a message sent to tso->msg_cap */
#define BlockedOnMsgThrowTo 12

/* The thread is not on any run queues, but can be woken up
   by tryWakeupThread() */
#define ThreadMigrating     13

-}
{-
/*
 * Constants for the what_next field of a TSO, which indicates how it
 * is to be run.
 */
#define ThreadRunGHC    1       /* return to address on top of stack */
#define ThreadInterpret 2       /* interpret this thread */
#define ThreadKilled    3       /* thread has died, don't run it */
#define ThreadComplete  4       /* thread has finished */
-}

