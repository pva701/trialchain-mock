module TrialChain.Arbitrary
       ( ValidTx (..)
       , arbitraryBalances
       , arbitraryValidTx
       , chooseSource
       , chooseDestination
       ) where

import Universum

import Data.List ((!!))
import qualified Data.Map as M
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

arbitraryBalances :: Gen Balances
arbitraryBalances = do
    num <- choose (1, 10)
    mkBalances <$> vectorOf num arbitrary

chooseSource :: Balances -> Gen (Address, Amount, Nonce)
chooseSource (Balances (M.toList -> bals)) = do
    let !num = length bals
    (txbSource, (srcBal, txbNonce)) <- (bals !!) <$> choose (0, num - 1)
    pure (txbSource, srcBal, txbNonce)

chooseDestination :: Balances -> Gen Address
chooseDestination (Balances (M.toList -> bals)) = do
    option :: Int <- choose (1, 2)
    if option == 1 then do
        let !num = length bals
        fst . (bals !!) <$> choose (0, num - 1)
    else
        arbitrary

arbitraryValidTx :: Balances -> Gen Tx
arbitraryValidTx balances = do
    (txbSource, srcBal, txbNonce) <- chooseSource balances
    txbDestination <- chooseDestination balances
    txbAmount <- Amount <$> choose (1, fromIntegral srcBal)
    Tx TxBody{..} <$> arbitrary <*> arbitrary

instance Arbitrary ValidTx where
    arbitrary = do
        vBalances <- arbitraryBalances
        vTx       <- arbitraryValidTx vBalances
        pure $ ValidTx {..}