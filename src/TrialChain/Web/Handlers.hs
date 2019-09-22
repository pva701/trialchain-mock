module TrialChain.Web.Handlers
       ( trialChainHandlers
       , WorkMode

       -- * For tests
       , postBroadcastTx
       , getTx
       ) where

import Universum

import qualified Data.Map as M
import Data.Persist (decode, encode)
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO as UIO

import TrialChain.App
import TrialChain.Web.API
import TrialChain.Web.Types
import TrialChain.Consensus
import TrialChain.Util

type TrialChainHandlers m = ToServant TrialChainEndpoints (AsServerT m)

-- | Abstract constraints for the methods.
type WorkMode env m
    = ( MonadReader env m
      , Has MempoolVar env
      , Has BalancesVar env
      , MonadUnliftIO m
      , WithError m
      )

-- | Server handler implementation for TrialChain API.
trialChainHandlers :: WorkMode env m => TrialChainHandlers m
trialChainHandlers = genericServerT trialChainEndpoints

trialChainEndpoints :: WorkMode env m => TrialChainEndpoints (AsServerT m)
trialChainEndpoints = TrialChainEndpoints
    { tceBroadcastTx = postBroadcastTx
    , tceGetTx       = getTx
    }

-- | Broadcast a transaction to mock blockchain.
-- Process it following our simple consensus algorithm.
postBroadcastTx :: forall env m . WorkMode env m => BroadcastTxRequest -> m BroadcastTxResponse
postBroadcastTx BroadcastTxRequest{..} = fmap (mkBroadcastTxResponse breqId) $ runExceptT $ do
    tx <- ExceptT $ pure $ first (TxDecodingFailed . toText) $ decode $ unHex breqRawTx
    balancesVar <- grab @BalancesVar @env
    mempoolVar <- grab @MempoolVar
    ExceptT $ UIO.atomically $ do
        balances <- UIO.readTVar balancesVar
        case verifyTxAndTransfer tx balances of
            Left e -> pure $ Left $ TxVerificationFailed e
            Right newBalances -> do
                UIO.writeTVar balancesVar newBalances
                UIO.modifyTVar mempoolVar (insertMempool tx)
                pure $ Right $ txId tx

-- | Get a transaction by its hash.
getTx :: WorkMode env m => GetTxRequest -> m GetTxResponse
getTx GetTxRequest {..} = fmap (mkGetTxResponse greqId) $ runExceptT $ do
    tId <- ExceptT $ pure $ first (TxIdDecodingFailed . toText) $ decode $ unHex greqTxId
    Mempool mempool <- UIO.readTVarIO =<< grab @MempoolVar
    case M.lookup tId mempool of
        Nothing -> ExceptT $ pure $ Left (TxIdNotFound tId)
        Just (tx, _orderNum) -> pure $ Hex $ encode tx