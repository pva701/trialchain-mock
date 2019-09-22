{- Types which relate to our simple consensus algorithm. -}

module TrialChain.Consensus.Types
       ( Nonce (..)
       , Amount (..)
       , TxBody (..)
       , Tx (..)
       , TxId
       , createTx
       , txId
       , Balances (..)
       , mkBalances
       , Mempool (..)
       , insertMempool
       ) where

import Universum

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Map as M
import Data.Persist (Persist (..))
import Fmt (Buildable (..))

import TrialChain.Crypto

-- | Nonce of a transaction.
-- Nonce is being stored for each address to prevent double spending.
newtype Nonce = Nonce Word32
    deriving (Eq, Ord, Show, Num, Generic, FromJSON, ToJSON, Persist, Buildable)

-- | Amount of money.
newtype Amount = Amount Word32
    deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Generic, FromJSON, ToJSON, Persist, Buildable)

-- | Transaction body.
data TxBody = TxBody
  { txbSource       :: Address
  -- ^ Source address, where money will be withdrawn from.
  , txbNonce        :: Nonce
  -- ^ Nonce of source address.
  , txbDestination :: Address
  -- ^ Destination address, where money will be sent to.
  , txbAmount       :: Amount
  -- ^ Amount of money to send.
  } deriving (Eq, Ord, Show, Generic)

instance Persist TxBody

type TxId = Hash TxBody

-- | Signed transaction consisting of a transcation body
-- and additional information to verify that a transaction
-- properlt signed.
data Tx = Tx
  { txBody      :: TxBody
  , txSourcePk  :: PublicKey
  , txSignature :: Signature TxBody
  } deriving (Eq, Ord, Show, Generic)

instance Persist Tx

-- | Create a tx from a secret key and a transaction body.
createTx :: SecretKey -> TxBody -> Tx
createTx sk raw = Tx raw (toPublicKey sk) (sign sk raw)

txId :: Tx -> TxId
txId = toHash . txBody

-- | Balances and nonces of addresses.
newtype Balances = Balances {unBalances :: Map Address (Amount, Nonce)}
    deriving (Eq, Show)

mkBalances :: [(Address, Amount)] -> Balances
mkBalances = Balances . M.fromList . map (second (,0))

-- | Transactions which have been sent so far.
-- Sequential number of a transaction is also held
-- to be able to build merkle-tree
-- or to be able to replay transactions.
-- Though this number isn't used within this task.
newtype Mempool = Mempool {unMempool :: Map TxId (Tx, Int)}

-- | Insert a tx to a mempool without transaction verification,
-- implying that given transaction has been already verified.
insertMempool :: Tx -> Mempool -> Mempool
insertMempool tx@(txId -> i) mp@(Mempool mempool) =
    if M.member i mempool then mp
    else Mempool $ M.insert i (tx, M.size mempool) mempool