{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Polysemy.Async
  ( -- * Effect
    Async (..)

    -- * Actions
  , async
  , await
  , cancel

    -- * Helpers
  , sequenceConcurrently

    -- * Interpretations
  , asyncToIOFinal
  , runAsync
  ) where

import           Data.Kind (Type)
import qualified Control.Concurrent.Async as A
import           Polysemy
import           Polysemy.Final



------------------------------------------------------------------------------
-- | An effect for spawning asynchronous computations.
--
-- The 'Maybe' returned by 'async' is due to the fact that we can't be sure an
-- 'Polysemy.Error.Error' effect didn't fail locally.
--
-- @since 0.5.0.0
data Async (h :: Type -> Type) m a where
  Async :: m a -> Async h m (h (Maybe a))
  Await :: h a -> Async h m a
  Cancel :: h a -> Async h m ()

makeSem ''Async


------------------------------------------------------------------------------
-- | Perform a sequence of effectful actions concurrently.
--
-- @since 1.2.2.0
sequenceConcurrently :: forall t h r a. (Traversable t, Member (Async h) r) =>
    t (Sem r a) -> Sem r (t (Maybe a))
sequenceConcurrently t = traverse (async @h) t >>= traverse await
{-# INLINABLE sequenceConcurrently #-}

------------------------------------------------------------------------------
-- | Run an 'Async' effect in terms of 'A.async' through final 'IO'.
--
-- /Beware/: Effects that aren't interpreted in terms of 'IO'
-- will have local state semantics in regards to 'Async' effects
-- interpreted this way. See 'Final'.
--
-- @since 1.2.0.0
asyncToIOFinal :: Member (Final IO) r
               => Sem (Async A.Async ': r) a
               -> Sem r a
asyncToIOFinal = interpretFinal $ \case
  Async m -> do
    ins <- getInspectorS
    m'  <- runS m
    liftS $ A.async (inspect ins <$> m')
  Await a -> liftS (A.wait a)
  Cancel a -> liftS (A.cancel a)
{-# INLINE asyncToIOFinal #-}

------------------------------------------------------------------------------
-- | Run an 'Async' effect purely.
--
-- @since 1.8.0.0
runAsync
  :: Sem (Async (Sem r) ': r) a
  -> Sem r a
runAsync = interpretH
    ( \case
      Async ma -> do
        is <- getInitialStateT
        ins <- getInspectorT
        sem <- runAsync <$> runT ma
        pure (inspect ins <$> sem <$ is)
      Await sem ->
        pureT =<< raise sem
      Cancel _ -> pureT ()
    )
{-# INLINE runAsync #-}
