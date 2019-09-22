module TrialChain.Arbitrary
       ( ValidTx (..)
       ) where

import Universum

import Data.List ((!!))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck (Arbitrary (..), choose, vectorOf)
import qualified Data.ByteString as BS

import TrialChain.Crypto
import TrialChain.Consensus

arbitraryByteString :: Int -> Gen ByteString
arbitraryByteString bytes = BS.pack <$> vectorOf bytes (choose (0, 255))

instance Arbitrary Address where
    arbitrary = Address <$> arbitraryByteString 32

instance Arbitrary PublicKey where
    arbitrary = PublicKey <$> arbitraryByteString 32

instance Arbitrary (Signature a) where
    arbitrary = Signature <$> arbitraryByteString 32

instance Arbitrary Amount where
    arbitrary = Amount <$> choose (1, 10000)

data ValidTx = ValidTx
  { vTx       :: Tx
  , vBalances :: Balances
  } deriving (Eq, Show)

instance Arbitrary ValidTx where
    arbitrary = do
        num <- choose (1, 10)
        bls <- vectorOf num arbitrary
        (txbSource, srcBal) <- (bls !!) <$> choose (0, num - 1)
        txbDestination <- fst . (bls !!) <$> choose (0, num - 1)
        txbAmount <- Amount <$> choose (1, fromIntegral srcBal)
        let txbNonce = 0
        vTx <- Tx TxBody{..} <$> arbitrary <*> arbitrary
        let vBalances = mkBalances bls
        pure $ ValidTx {..}