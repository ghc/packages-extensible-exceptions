
#if __GLASGOW_HASKELL__ >= 609
module Control.Exception.Extensible (module Control.Exception) where

import Control.Exception

#else
module Control.Exception.Extensible (
    Exception(..),
    SomeException(..),
    throwIO,
    throw,
    throwTo,
    catch,
    try,
    E.block,
    E.unblock,
    E.evaluate,
    bracket,
    onException,
    finally,
    E.ArithException(..),
    E.ArrayException(..),
    AssertionFailed(..),
    E.AsyncException(..),
    BlockedOnDeadMVar(..),
    BlockedIndefinitely(..),
    NestedAtomically(..),
    Deadlock(..),
    ErrorCall(..),
    ExitCode(..),
    E.IOException,
    NoMethodError(..),
    NonTermination(..),
    PatternMatchFail(..),
    RecConError(..),
    RecSelError(..),
    RecUpdError(..)
    ) where

import Prelude hiding (catch)
import Control.Concurrent hiding (throwTo)
import qualified Control.Exception as E
import Data.Dynamic
import Data.Typeable
import System.Exit

class (Typeable e, Show e) => Exception e where
    toException   :: e -> SomeException
    fromException :: SomeException -> Maybe e

    toException = SomeException
    fromException (SomeException e) = cast e

data SomeException = forall e . Exception e => SomeException e
    deriving Typeable

instance Show SomeException where
    showsPrec p (SomeException e) = showsPrec p e

instance Exception SomeException where
    toException se = se
    fromException = Just

mkOldException :: Exception e => e -> E.Exception
mkOldException e = let e' = toException e
          in case fromException e' of
             Just e'' -> -- If the exception is actually a legacy exception
                         -- then throw it directly so the legacy functions
                         -- catch it as they expect
                         e''
             Nothing -> -- Otherwise, throw it as a dynamic
                        E.DynException (toDyn e')

throw :: Exception e => e -> a
throw e = E.throw (mkOldException e)

throwIO :: Exception e => e -> IO a
throwIO e = E.throwIO (mkOldException e)

throwTo :: Exception e => ThreadId -> e -> IO ()
throwTo tid e = E.throwTo tid (mkOldException e)

-- XXX Dyn
catch :: Exception e => IO a -> (e -> IO a) -> IO a
catch io handler = io `E.catch` handler'
    where handler' e = case fromException (toException e) of
                       Just e' ->
                           -- Handle the case where e == E.Exception,
                           -- or one of the types that make up E.Exception
                           handler e'
                       Nothing ->
                           case e of
                           E.DynException dyn ->
                               case fromDynamic dyn of
                               Just (SomeException exc) ->
                                   case cast exc of
                                   Just e' ->
                                       -- Handle the case where we have
                                       -- a new exception type encoded
                                       -- as a Dynamic
                                       handler e'
                                   Nothing -> E.throw e
                               Nothing -> E.throw e
                           _ -> E.throw e

bracket
        :: IO a         -- ^ computation to run first (\"acquire resource\")
        -> (a -> IO b)  -- ^ computation to run last (\"release resource\")
        -> (a -> IO c)  -- ^ computation to run in-between
        -> IO c         -- returns the value from the in-between computation
bracket before after thing =
  E.block (do
    a <- before
    r <- E.unblock (thing a) `onException` after a
    after a
    return r
 )

onException :: IO a -> IO b -> IO a
onException io what = io `catch` \e -> do what
                                          throw (e :: SomeException)

finally :: IO a         -- ^ computation to run first
        -> IO b         -- ^ computation to run afterward (even if an exception
                        -- was raised)
        -> IO a         -- returns the value from the first computation
a `finally` sequel =
  E.block (do
    r <- E.unblock a `onException` sequel
    sequel
    return r
  )

try :: Exception e => IO a -> IO (Either e a)
try a = catch (a >>= \ v -> return (Right v)) (\e -> return (Left e))

----------------------------------------------------------------------
-- Exception instance for the legacy Exception type

instance Exception E.Exception

----------------------------------------------------------------------
-- The new Exception types. These need to map to/from E.Exception so
-- that uses of legacy catch/throw functions work.

----

instance Exception E.ArithException where
    toException ae = toException (E.ArithException ae)
    fromException (SomeException e) = case cast e of
                                      Just (E.ArithException ae) ->
                                         Just ae
                                      _ -> Nothing
----

instance Exception E.ArrayException where
    toException ae = toException (E.ArrayException ae)
    fromException (SomeException e) = case cast e of
                                      Just (E.ArrayException ae) ->
                                          Just ae
                                      _ -> Nothing

----

data AssertionFailed = AssertionFailed String
    deriving Typeable

instance Exception AssertionFailed where
    toException (AssertionFailed str) = toException (E.AssertionFailed str)
    fromException (SomeException e) = case cast e of
                                      Just (E.AssertionFailed str) ->
                                          Just (AssertionFailed str)
                                      _ -> Nothing

instance Show AssertionFailed where
    showsPrec _ (AssertionFailed err) = showString err

-----

instance Exception E.AsyncException where
    toException ae = toException (E.AsyncException ae)
    fromException (SomeException e) = case cast e of
                                      Just (E.AsyncException ae) ->
                                          Just ae
                                      _ -> Nothing

----

data BlockedOnDeadMVar = BlockedOnDeadMVar
    deriving Typeable

instance Exception BlockedOnDeadMVar where
    toException BlockedOnDeadMVar = toException (E.BlockedOnDeadMVar)
    fromException (SomeException e) = case cast e of
                                      Just E.BlockedOnDeadMVar ->
                                          Just BlockedOnDeadMVar
                                      _ -> Nothing
instance Show BlockedOnDeadMVar where
    showsPrec n BlockedOnDeadMVar = showsPrec n E.BlockedOnDeadMVar

----

data BlockedIndefinitely = BlockedIndefinitely
    deriving Typeable

instance Exception BlockedIndefinitely where
    toException BlockedIndefinitely = toException E.BlockedIndefinitely
    fromException (SomeException e) = case cast e of
                                      Just E.BlockedIndefinitely ->
                                          Just BlockedIndefinitely
                                      _ -> Nothing

instance Show BlockedIndefinitely where
    showsPrec n BlockedIndefinitely = showsPrec n E.BlockedIndefinitely

----

data NestedAtomically = NestedAtomically
    deriving Typeable

instance Exception NestedAtomically where
    toException NestedAtomically = toException E.NestedAtomically
    fromException (SomeException e) = case cast e of
                                    Just E.NestedAtomically ->
                                        Just NestedAtomically
                                    _ -> Nothing

instance Show NestedAtomically where
    showsPrec n NestedAtomically = showsPrec n E.NestedAtomically

----

data Deadlock = Deadlock
    deriving Typeable

instance Exception Deadlock where
    toException Deadlock = toException E.Deadlock
    fromException (SomeException e) = case cast e of
                                      Just E.Deadlock ->
                                          Just Deadlock
                                      _ -> Nothing

instance Show Deadlock where
    showsPrec n Deadlock = showsPrec n E.Deadlock

-----

data ErrorCall = ErrorCall String
    deriving Typeable

instance Exception ErrorCall where
    toException (ErrorCall str) = toException (E.ErrorCall str)
    fromException (SomeException e) = case cast e of
                                      Just (E.ErrorCall str) ->
                                          Just (ErrorCall str)
                                      _ -> Nothing

instance Show ErrorCall where
    showsPrec _ (ErrorCall err) = showString err

-----

instance Typeable ExitCode where
    typeOf _ = mkTyConApp (mkTyCon "ExitCode") []

instance Exception ExitCode where
    toException ee = toException (E.ExitException ee)
    fromException (SomeException e) = case cast e of
                                      Just (E.ExitException ee) ->
                                          Just ee
                                      _ -> Nothing
-----

instance Exception E.IOException where
    toException ioe = toException (E.IOException ioe)
    fromException (SomeException e) = case cast e of
                                      Just (E.IOException ioe) ->
                                          Just ioe
                                      _ -> Nothing

----

data NoMethodError = NoMethodError String
    deriving Typeable

instance Exception NoMethodError where
    toException (NoMethodError str) = toException (E.NoMethodError str)
    fromException (SomeException e) = case cast e of
                                      Just (E.NoMethodError str) ->
                                          Just (NoMethodError str)
                                      _ -> Nothing

instance Show NoMethodError where
    showsPrec _ (NoMethodError str) = showString str

----

data NonTermination = NonTermination
    deriving Typeable

instance Exception NonTermination where
    toException NonTermination = toException E.NonTermination
    fromException (SomeException e) = case cast e of
                                      Just E.NonTermination ->
                                          Just NonTermination
                                      _ -> Nothing

instance Show NonTermination where
    showsPrec n NonTermination = showsPrec n E.NonTermination

----

data PatternMatchFail = PatternMatchFail String
    deriving Typeable

instance Exception PatternMatchFail where
    toException (PatternMatchFail str) = toException (E.PatternMatchFail str)
    fromException (SomeException e) = case cast e of
                                      Just (E.PatternMatchFail str) ->
                                          Just (PatternMatchFail str)
                                      _ -> Nothing

instance Show PatternMatchFail where
    showsPrec _ (PatternMatchFail str) = showString str
    

----

data RecConError = RecConError String
    deriving Typeable

instance Exception RecConError where
    toException (RecConError str) = toException (E.RecConError str)
    fromException (SomeException e) = case cast e of
                                      Just (E.RecConError str) ->
                                          Just (RecConError str)
                                      _ -> Nothing

instance Show RecConError where
    showsPrec _ (RecConError str) = showString str
    


----

data RecSelError = RecSelError String
    deriving Typeable

instance Exception RecSelError where
    toException (RecSelError str) = toException (E.RecSelError str)
    fromException (SomeException e) = case cast e of
                                      Just (E.RecSelError str) ->
                                          Just (RecSelError str)
                                      _ -> Nothing

instance Show RecSelError where
    showsPrec _ (RecSelError str) = showString str

----

data RecUpdError = RecUpdError String
    deriving Typeable

instance Exception RecUpdError where
    toException (RecUpdError str) = toException (E.RecUpdError str)
    fromException (SomeException e) = case cast e of
                                      Just (E.RecUpdError str) ->
                                          Just (RecUpdError str)
                                      _ -> Nothing

instance Show RecUpdError where
    showsPrec _ (RecUpdError str) = showString str


#endif

