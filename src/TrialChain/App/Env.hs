module TrialChain.App.Env
       ( Env (..)
       , newEmptyEnv
       , MempoolVar
       , BalancesVar
       , Has (..)
       , grab
       ) where

import Universum

import qualified UnliftIO as UIO

import TrialChain.Consensus.Types

type MempoolVar = TVar Mempool
type BalancesVar = TVar Balances

data Env = Env
  { envMempool  :: MempoolVar
  , envBalances :: BalancesVar
  }

newEmptyEnv :: MonadIO m => m Env
newEmptyEnv = Env <$> UIO.newTVarIO (Mempool mempty) <*> UIO.newTVarIO (Balances mempty)

class Has field env where
  obtain :: env -> field

instance Has MempoolVar Env where
  obtain = envMempool

instance Has BalancesVar Env where
  obtain = envBalances

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}