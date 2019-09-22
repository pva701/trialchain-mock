module TrialChain.App.Env
       ( Env (..)
       , newEnv
       , defaultBalances
       , MempoolVar
       , BalancesVar
       , Has (..)
       , grab
       ) where

import Universum

import qualified UnliftIO as UIO

import TrialChain.Consensus.Types
import TrialChain.Crypto

type MempoolVar = TVar Mempool
type BalancesVar = TVar Balances

data Env = Env
  { envMempool  :: MempoolVar
  , envBalances :: BalancesVar
  }

newEnv :: MonadIO m => Balances -> m Env
newEnv bals = Env <$> UIO.newTVarIO (Mempool mempty) <*> UIO.newTVarIO bals

defaultBalances :: [(Address, Amount)]
defaultBalances =
    [ (unsafeParseAddress "abcdef", 100)
    , (unsafeParseAddress "10bcaf", 200)
    , (unsafeParseAddress "abdffe0123", 300)
    ]

class Has field env where
  obtain :: env -> field

instance Has MempoolVar Env where
  obtain = envMempool

instance Has BalancesVar Env where
  obtain = envBalances

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}