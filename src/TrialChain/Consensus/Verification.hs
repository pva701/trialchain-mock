{- Verification of rules of the consensus algorithm. -}

module TrialChain.Consensus.Verification
       ( TxVerificationError (..)
       , verifyTxAndTransfer
       ) where

import Universum

import Data.Aeson (ToJSON (..))
import qualified Data.Map as M
import Control.Monad.Except (throwError)
import Fmt (Buildable (..), (+|), (|+))

import TrialChain.Crypto
import TrialChain.Consensus.Types
import TrialChain.Util

-- | Possible errors which may happen during a transaction verification.
data TxVerificationError
    = TxInvalidSignature TxId (Signature TxBody)
    | TxZeroAmount TxId
    | SourceAddressNotFound Address
    | NotEnoughBalance Address Amount Amount
    | NonceMismatch Address Nonce Nonce
    deriving (Eq, Show)

instance Buildable TxVerificationError where
    build (TxInvalidSignature tId sig) = "Transaction #" +| tId |+ " is signed incorrectly with " +| sig |+ "."
    build (TxZeroAmount tId) = "Transaction #" +| tId |+ " transfers zero amount."
    build (SourceAddressNotFound source) = "Source address " +| source |+ " doesn't exist."
    build (NotEnoughBalance source bal amount) =
        "Balance of " +| source |+ " is " +| bal |+ ", what is less than required amount " +| amount |+ "."
    build (NonceMismatch source nonceEx nonceTx) =
        "Nonce mismatch for source address " +| source |+ ": expected is " +| nonceEx |+ ", transaction one is " +| nonceTx |+ "."

instance ToJSON TxVerificationError where
    toJSON = toJSON . JsonViaBuildable

-- | Validate a transaction signature.
verifyTxSignature :: Tx -> Bool
verifyTxSignature Tx{..} = verifySignature txBody txSignature txSourcePk

-- | Apply a transaction to a balance storage,
-- verifying consensus rules before that.
transfer :: TxBody -> Balances -> Either TxVerificationError Balances
transfer TxBody{..} (Balances bals) = do
    (balance, nonce) <-
        maybe (throwError $ SourceAddressNotFound txbSource)
              pure
              (M.lookup txbSource bals)
    unless (balance >= txbAmount) $
        throwError $ NotEnoughBalance txbSource balance txbAmount
    unless (nonce == txbNonce) $
        throwError $ NonceMismatch txbSource nonce txbNonce

    let bals1 = M.insert txbSource (balance - txbAmount, nonce + 1) bals
    let (dstBalance, dstNonce) = fromMaybe (0, 0) (M.lookup txbDestination bals1)
    pure $ Balances $ M.insert txbDestination (dstBalance + txbAmount, dstNonce) bals1

-- | Verify all consensus rules and apply a transaction then.
verifyTxAndTransfer :: Tx -> Balances -> Either TxVerificationError Balances
verifyTxAndTransfer tx@Tx{..} balances = do
    unless (verifyTxSignature tx) $
        throwError $ TxInvalidSignature (txId tx) txSignature
    unless (txbAmount txBody /= 0) $
        throwError $ TxZeroAmount (txId tx)
    transfer txBody balances