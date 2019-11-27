{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Trace
  ( MonadError (..)
  , MonadTrace (..)
  , Trace, TraceT (..)
  , runTrace, runTraceT
  , mapTrace, mapTraceT
  , mapError, mapErrorT
  , showStackTrace
  ) where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Fail
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Control
import Control.Applicative

-- | Class of monads that can be run in some annotated environment, analogous to a stack frame.
class MonadError e m => MonadTrace e s m | m -> e, m -> s where
  -- | Run an action, prepending @s@ to the stack.
  --
  -- > traceError "action a" $ do
  -- >   actionA
  -- >   traceError "action b" $ do
  -- >     actionB
  --
  traceError :: s -> m a -> m a

instance Monad m => MonadError e (TraceT e s m) where
    throwError e = TraceT $ throwError (e,[])
    catchError (TraceT m) cont = TraceT $ catchError m (\(e,_) -> unTrace $ cont e )

instance Monad m => MonadTrace s e (TraceT s e m) where
    traceError s (TraceT except) = TraceT $ withExceptT (\(e,ss) -> (e,s:ss)) except

-- default version of traceError for monad transformers with a MonadTransControl instance
mtcTraceError :: (MonadTransControl t, MonadError e (t m), MonadTrace e s m)
              => s -> t m a -> t m a
mtcTraceError frame inner = liftWith (\run -> traceError frame (run inner)) >>= restoreT . pure

instance MonadTrace e s m => MonadTrace e s (StateT q m)  where traceError = mtcTraceError
instance MonadTrace e s m => MonadTrace e s (ReaderT r m) where traceError = mtcTraceError
instance MonadTrace e s m => MonadTrace e s (IdentityT m) where traceError = mtcTraceError
instance MonadTrace e s m => MonadTrace e s (MaybeT m)    where traceError = mtcTraceError
instance (Monoid w, MonadTrace e s m) => MonadTrace e s (RWST r w q m)where traceError = mtcTraceError

-- | The stack trace monad transformer.
--   Augmented version of 'ExceptT', that fails with an error and a stack of enclosing 'traceError' frames.
--   To fail, use 'throwError'.
--
--   This might get changed, but currently 'catchError' will destroy the enclosed action's stack trace.
newtype TraceT e s m a = TraceT {unTrace :: ExceptT (e,[s]) m a}
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFail, Eq, Show)

deriving instance MonadState  s m => MonadState  s (TraceT e q m)
deriving instance MonadReader r m => MonadReader r (TraceT e s m)
deriving instance MonadWriter w m => MonadWriter w (TraceT e s m)
deriving instance MonadRWS r w s m => MonadRWS r w s (TraceT e st m)

deriving instance (Monad m, Monoid e) => Alternative (TraceT e s m)
deriving instance (Monad m, Monoid e) => MonadPlus (TraceT e s m)

type Trace e s a = TraceT e s Identity a

-- | Run a `TraceT` and its enclosed `ExceptT`.
runTraceT :: TraceT e s m a -> m (Either (e,[s]) a)
runTraceT (TraceT m) = runExceptT m

runTrace :: Trace e s a -> Either (e,[s]) a
runTrace = runIdentity . runTraceT

-- | Modify a `TraceT`'s base monad, error/stack type, and/or result value.
mapTraceT :: (m (Either (e,[s]) a) -> n (Either (e',[s']) b))
          -> TraceT e s m a
          -> TraceT e' s' n b
mapTraceT f (TraceT m) = TraceT $ mapExceptT f m

mapTrace :: (Either (e,[s]) a -> Either (e',[s']) b)
         -> Trace e s a
         -> Trace e' s' b
mapTrace f (TraceT m) = TraceT $ mapExcept f m

mapErrorT :: Functor m
          => (e -> e')
          -> TraceT e  s m a
          -> TraceT e' s m a
mapErrorT f (TraceT m) = TraceT $ withExceptT (\(e,s) -> (f e,s)) m

mapError :: (e -> e')
         -> Trace e  s a
         -> Trace e' s a
mapError = mapErrorT

-- | Efficiently print a stack trace.
--   Function arguments take an additional 'Int' argument indicating their depth.
--   This can be useful to e.g. prepend spaces to show an indented trace.
showStackTrace :: (Int -> s -> ShowS)
               -> (Int -> e -> ShowS)
               -> (e,[s])
               -> String
showStackTrace fs fe (e,st) = go st 0 ""
  where
    go (s:ss) n = fs n s . go ss (n+1)
    go []     n = fe n e
