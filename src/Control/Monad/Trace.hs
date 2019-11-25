{-# LANGUAGE OverloadedStrings #-}
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
import Control.Applicative

class MonadError e m => MonadTrace e s m | m -> e, m -> s where
  traceError :: s -> m a -> m a

instance Monad m => MonadError e (TraceT e s m) where
    throwError e = TraceT $ throwError (e,[])
    catchError (TraceT m) cont = TraceT $ catchError m (\(e,_) -> unTrace $ cont e )

instance Monad m => MonadTrace s e (TraceT s e m) where
    traceError s (TraceT except) = TraceT $ withExceptT (\(e,ss) -> (e,s:ss)) except

instance MonadTrace e q m => MonadTrace e q (StateT s m)  where traceError s m = get >>= lift . traceError s . evalStateT m
instance MonadTrace e s m => MonadTrace e s (ReaderT r m) where traceError s m = ask >>= lift . traceError s . runReaderT m
instance MonadTrace e s m => MonadTrace e s (IdentityT m) where traceError s m = IdentityT $ traceError s (runIdentityT m)
instance MonadTrace e s m => MonadTrace e s (MaybeT m)    where traceError s m = MaybeT    $ traceError s (runMaybeT m)

newtype TraceT e s m a = TraceT {unTrace :: ExceptT (e,[s]) m a}
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFail, Eq, Show)

deriving instance MonadState  s m => MonadState  s (TraceT e q m)
deriving instance MonadReader r m => MonadReader r (TraceT e s m)
deriving instance MonadWriter w m => MonadWriter w (TraceT e s m)
deriving instance MonadRWS r w s m => MonadRWS r w s (TraceT e st m)

deriving instance (Monad m, Monoid e) => Alternative (TraceT e s m)
deriving instance (Monad m, Monoid e) => MonadPlus (TraceT e s m)

type Trace e s a = TraceT e s Identity a

runTraceT :: TraceT e s m a -> m (Either (e,[s]) a)
runTraceT (TraceT m) = runExceptT m

runTrace :: Trace e s a -> Either (e,[s]) a
runTrace = runIdentity . runTraceT

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

showStackTrace :: (Int -> s -> ShowS) -> (Int -> e -> ShowS) -> (e,[s]) -> String
showStackTrace fs fe (e,st) = go st 0 ""
  where
    go (s:ss) n = fs n s . go ss (n+1)
    go []     n = fe n e
