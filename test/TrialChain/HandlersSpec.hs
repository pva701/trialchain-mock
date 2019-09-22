module TrialChain.HandlersSpec
       ( spec
       ) where

import Universum

import Data.Persist (encode)
import qualified Data.Map as M
import Fmt (pretty, fmt, (+|), (|+), blockListF)
import qualified UnliftIO.Async as UIO
import qualified UnliftIO as UIO
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck.Property (ioProperty, (===), Property)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck (property, (.&&.), choose, shuffle, Arbitrary (..), counterexample)
import Test.QuickCheck.Monadic (monadicIO, pick)

import TrialChain.Arbitrary
import TrialChain.Web
import TrialChain.App
import TrialChain.Util
import TrialChain.Consensus
import TrialChain.Crypto
import TrialChain.TestApp

spec :: Spec
spec = describe "Handlers tests" $ do
    it "Consistency test (get_tx after broadcast)" $ property $ \ValidTx{..} -> ioProperty $ do
        env <- newEnv vBalances
        runApp env $ do
            broadcastTxResponse <- postBroadcastTx BroadcastTxRequest
                { breqId    = 1
                , breqRawTx = Hex $ encode vTx
                }
            whenJust (bresError broadcastTxResponse) $ \e ->
                throwError $ InternalError $ pretty e
            tId <- throwOnNothing (InternalError "/broadcast has to return TxId") (bresResult broadcastTxResponse)

            getTxResponse <- getTx GetTxRequest
                { greqId = 2
                , greqTxId = Hex $ encode tId
                }
            whenJust (gresError getTxResponse) $ \e ->
                throwError $ InternalError $ pretty e
            txHex <- throwOnNothing (InternalError "/get_tx has to return Tx") (gresResult getTxResponse)
            pure $
                bresId broadcastTxResponse === (1 :: Int) .&&.
                gresId getTxResponse === (2 :: Int) .&&.
                txHex === Hex (encode vTx)

    -- This tets checks that having several concurrent transactions with
    -- the same source address and nonce, only one will be accepted.
    -- The test happens in several rounds (up to 10). One each round:
    -- 1. Several source addresses, which are not run out of money, are being chosen (up to 5).
    -- 2. For each source address, group of transactions having this source and the same nonce
    --    are being generated (up to 10).
    -- 3. All transactions are run concurrently.
    -- 4. Perform check that exactly one transaction in each group has been accepted.
    it "Concurrent /broadcast" $ monadicIO $ do
        initBalances <- pick arbitraryBalances
        rounds <- pick $ choose (1, 10)
        hoistTestApp initBalances $ fmap propertyMConcat $ forM [1..rounds] $ \roundNum -> do
            balances <- lift $ UIO.readTVarIO =<< grab @BalancesVar
            (groupedTxs, allTxs) <- pick $ genConcurrentTxs balances
            lift $ UIO.forConcurrently_ allTxs $ \tx -> void $
                postBroadcastTx BroadcastTxRequest
                    { breqId    = 1
                    , breqRawTx = Hex $ encode tx
                    }
            lift $ checkInvariant roundNum groupedTxs

genConcurrentTxs
   :: Balances
   -> Gen ( [(Address, [Tx])] -- ^ address and transactions with source equals to the address
          , [Tx]              -- ^ shuffled trasactions
          )
genConcurrentTxs balances = do
    let positiveBalances = filter ((/= 0) . fst . snd) $ M.toList $ unBalances balances
    concurrentSourcesNum <- choose (1, min 5 (length positiveBalances))
    concurrentSources <- take concurrentSourcesNum <$> shuffle positiveBalances
    txsPerSource <- choose (1, 10)
    txsGrouped <- mapM (genTxsForSource txsPerSource) concurrentSources
    allTxs <- shuffle (concat txsGrouped)
    pure (zip (map fst concurrentSources) txsGrouped, allTxs)
  where
    genTxsForSource txsNum (txbSource, (srcBal, txbNonce)) = do
        bodies <- fmap ordNub $ replicateM txsNum $ do
            txbDestination <- chooseDestination balances
            txbAmount <- Amount <$> choose (1, fromIntegral srcBal)
            pure TxBody{..}
        mapM (\body -> Tx body <$> arbitrary <*> arbitrary) bodies

checkInvariant :: Int -> [(Address, [Tx])] -> App Property
checkInvariant roundNum allTxs = fmap propertyMConcat $ do
    results <- UIO.forConcurrently allTxs $ \(_source, txs) ->
        forM txs $ \tx -> do
            getTxResponse <- getTx GetTxRequest
                { greqId = 2
                , greqTxId = Hex $ encode (txId tx)
                }
            if isJust (gresResult getTxResponse) then pure $ Just tx
            else pure Nothing
    pure $ map toProperty results
  where
    toProperty (catMaybes -> xs) = counterexample (fmt $ buildable xs) (length xs == 1)
    buildable xs =
        "Failed at the round " +| roundNum |+ ". "
        +| "Transactions:\n" +| blockListF (map txBody xs) |+ ""

propertyMConcat :: [Property] -> Property
propertyMConcat = foldl (.&&.) (property True)