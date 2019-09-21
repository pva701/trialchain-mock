module TrialChain.Web.API
       ( TrialChainEndpoints (..)
       , TrialChainAPI
       , trialChainAPI
       ) where

import Universum

import Servant.API ((:>), JSON, Get, Post, ReqBody)
import Servant.API.Generic ((:-), AsApi, ToServant)

import TrialChain.Web.Types

data TrialChainEndpoints route = TrialChainEndpoints
    { tceBroadcastTx :: route
        :- "broadcast"
        :> ReqBody '[JSON] BroadcastTxRequest
        :> Post '[JSON] BroadcastTxResponse
    , tceGetTx :: route
        :- "get_tx"
        :> ReqBody '[JSON] GetTxRequest
        :> Get '[JSON] GetTxResponse
    } deriving (Generic)

-- | API type specification.
type TrialChainAPI =
    "api" :> ToServant TrialChainEndpoints AsApi

trialChainAPI :: Proxy TrialChainAPI
trialChainAPI = Proxy
