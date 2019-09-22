module TrialChain.ConsensusSpec
       ( spec
       ) where

import Universum

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck.Property ((===))
import Test.QuickCheck (arbitrary, choose, Arbitrary (..), property)
import Test.QuickCheck.Gen (Gen (..))

import TrialChain.Consensus
import TrialChain.Arbitrary
import TrialChain.Util
import TrialChain.Crypto

spec :: Spec
spec = describe "Consensus rules" $ do
    it "Not enough balance rule" $ property $ \(invalid :: InvalidTx NotEnoughBalance) ->
        let source = txbSource $ txBody $ iTx invalid in
        let [(balance, _)] = toList (unBalances $ iBalances invalid) in
        runInvalid invalid === Left (NotEnoughBalance source balance (txbAmount $ txBody $ iTx invalid))

    it "Zero amount rule" $ property $ \(invalid :: InvalidTx ZeroAmount) ->
        runInvalid invalid === Left (TxZeroAmount $ txId $ iTx invalid)

    it "Nonce mismatch rule" $ property $ \(invalid :: InvalidTx NonceMismatch ) ->
        runInvalid invalid ===
            Left (NonceMismatch (txbSource $ txBody $ iTx invalid) 0 (txbNonce $ txBody $ iTx invalid))

    it "Source not found rule" $ property $ \(invalid :: InvalidTx UnknownSourceAddress) ->
        runInvalid invalid === Left (SourceAddressNotFound $ txbSource $ txBody $ iTx invalid)

    it "Invalid tx signature rule" $ property $ \(invalid :: InvalidTx IllSignature) ->
        runInvalid invalid === Left (TxInvalidSignature (txId $ iTx invalid) (txSignature $ iTx invalid))

    it "Valid transaction" $ property $ \ValidTx{..} ->
        second (const ()) (verifyTxAndTransfer vTx vBalances) === Right ()
  where
    runInvalid invalid = verifyTxAndTransfer (iTx invalid) (iBalances invalid)

data NotEnoughBalance
data ZeroAmount
data NonceMismatch
data UnknownSourceAddress
data IllSignature

data InvalidTx c = InvalidTx
    { iTx       :: Tx
    , iBalances :: Balances
    } deriving (Eq, Show)

arbitraryAddresses :: Nonce -> Amount -> Gen TxBody
arbitraryAddresses txbNonce txbAmount = do
    txbSource      <- arbitrary
    txbDestination <- arbitrary
    pure $ TxBody{..}

instance Arbitrary (InvalidTx NotEnoughBalance) where
    arbitrary = do
        balance <- Amount <$> choose (1, 100)
        amount <- Amount <$> choose (fromIntegral balance + 1, 200)
        txBody@TxBody{..} <- arbitraryAddresses 0 amount
        let iBalances = mkBalances $ one (txbSource, balance)
        iTx <- Tx txBody <$> arbitrary <*> arbitrary
        pure $ InvalidTx{..}

instance Arbitrary (InvalidTx ZeroAmount) where
    arbitrary = do
        txBody@TxBody{..} <- arbitraryAddresses 0 0
        sourceBalance <- Amount <$> choose (1, 100)
        dstBalance <- Amount <$> choose (1, 100)
        let iBalances = mkBalances [(txbSource, sourceBalance), (txbDestination, dstBalance)]
        iTx <- Tx txBody <$> arbitrary <*> arbitrary
        pure $ InvalidTx{..}

instance Arbitrary (InvalidTx NonceMismatch) where
    arbitrary = do
        sourceBalance <- Amount <$> choose (1, 100)
        amount <- Amount <$> choose (1, fromIntegral sourceBalance)
        dstBalance <- Amount <$> choose (1, 100)
        txBody@TxBody{..} <- flip arbitraryAddresses amount =<< (Nonce <$> choose (1, 10))
        let iBalances = mkBalances [(txbSource, sourceBalance), (txbDestination, dstBalance)]
        iTx <- Tx txBody <$> arbitrary <*> arbitrary
        pure $ InvalidTx {..}

instance Arbitrary (InvalidTx UnknownSourceAddress) where
    arbitrary = do
        sourceBalance <- Amount <$> choose (1, 100)
        txbAmount <- Amount <$> choose (1, fromIntegral sourceBalance)
        txBody <- arbitraryAddresses 0 txbAmount
        let iBalances = mkBalances []
        iTx <- Tx txBody <$> arbitrary <*> arbitrary
        pure $ InvalidTx{..}

instance Arbitrary (InvalidTx IllSignature) where
    arbitrary = do
        sourceBalance <- Amount <$> choose (1, 100)
        amount <- Amount <$> choose (1, fromIntegral sourceBalance)
        dstBalance <- Amount <$> choose (1, 100)

        txBody@TxBody{..} <- arbitraryAddresses 0 amount
        let iBalances = mkBalances [(txbSource, sourceBalance), (txbDestination, dstBalance)]
        iTx <- Tx txBody <$> arbitrary <*> pure (Signature $ unsafeReadHex "ffffffffff")
        pure $ InvalidTx{..}