{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DerivingVia #-}

module TrialChain.Web.Types
       ( BroadcastTxRequest (..)
       , BroadcastTxResponse
       , bresId
       , bresResult
       , bresError
       , mkBroadcastTxResponse
       , BroadcastTxResponseError (..)
       , GetTxRequest (..)
       , GetTxResponse
       , gresId
       , gresResult
       , gresError
       , mkGetTxResponse
       , GetTxResponseError (..)
       ) where

import Universum

import Data.Aeson (ToJSON (..))
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import Fmt (Buildable (..), (+|), (|+), pretty)

import TrialChain.Consensus
import TrialChain.Util

-- | /broadcast request params.
data BroadcastTxRequest = BroadcastTxRequest
  { breqId    :: Int
  -- ^ Request id.
  , breqRawTx :: Hex Tx
  -- ^ Hex value of a raw tx.
  }

-- | /broadcast response.
data BroadcastTxResponse = BroadcastTxResponse
  { bresId      :: Int
  -- ^ Corresponding request id.
  , bresResult  :: Maybe TxId
  -- ^ Transaction id.
  , bresError   :: Maybe BroadcastTxResponseError
  -- ^ An error which happened during request.
  }

-- | Smart constructor for BroadcastTxResponse
mkBroadcastTxResponse :: Int -> Either BroadcastTxResponseError TxId -> BroadcastTxResponse
mkBroadcastTxResponse bresId (Left e)    = BroadcastTxResponse bresId Nothing (Just e)
mkBroadcastTxResponse bresId (Right tId) = BroadcastTxResponse bresId (Just tId) Nothing

-- | Possible errors which may hapen during /broadcast request.
data BroadcastTxResponseError
    = TxVerificationFailed TxVerificationError
    -- ^ Error if a transaction doesn't satisfy consensus rules.
    | TxDecodingFailed     Text
    -- ^ Error if raw bytes don't correspond to encoded transaction.

instance Buildable BroadcastTxResponseError where
    build (TxVerificationFailed reason) = pretty reason
    build (TxDecodingFailed reason)     = "Decoding failed: " +| reason |+ "."

instance ToJSON BroadcastTxResponseError where
    toJSON = toJSON . JsonViaBuildable

-- | /get_tx request params.
data GetTxRequest = GetTxRequest
  { greqId      :: Int
    -- ^ Request id.
  , greqTxId :: Hex TxId
    -- ^ A transaction hash.
  }

-- | /get_tx response.
data GetTxResponse = GetTxResponse
  { gresId     :: Int
  -- ^ Corresponding request id.
  , gresResult :: Maybe (Hex Tx)
  -- ^ Raw transaction in hexadecimal format.
  , gresError  :: Maybe GetTxResponseError
  }

-- | Smart constructor for GetTxResponse
mkGetTxResponse :: Int -> Either GetTxResponseError (Hex Tx) -> GetTxResponse
mkGetTxResponse gresId (Left e)   = GetTxResponse gresId Nothing (Just e)
mkGetTxResponse gresId (Right tx) = GetTxResponse gresId (Just tx) Nothing

-- | Possible errors which may hapen during /get_tx request.
data GetTxResponseError
    = TxIdNotFound TxId
    -- ^ Error if a transaction with given id not found.
    | TxIdDecodingFailed Text
    -- ^ Error if raw bytes don't correspond to encoded hash of transaction.

instance Buildable GetTxResponseError where
    build (TxIdNotFound tId) = "Tx #" +| tId |+ " not found."
    build (TxIdDecodingFailed reason) = "Decoding failed: " +| reason |+ "."

instance ToJSON GetTxResponseError where
    toJSON = toJSON . JsonViaBuildable

deriveFromJSON defaultOptions ''BroadcastTxRequest
deriveToJSON   defaultOptions ''BroadcastTxResponse
deriveFromJSON defaultOptions ''GetTxRequest
deriveToJSON   defaultOptions ''GetTxResponse