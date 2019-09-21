module TrialChain.Web.API
       ( TrialChainEndpoints (..)
       , TrialChainAPI
       , trialChainAPI
       ) where

import Universum

import Servant.API ((:>), JSON, StdMethod (..), Verb, QueryParam)
import Servant.API.Generic ((:-), AsApi, ToServant)

data TrialChainEndpoints route = TrialChainEndpoints
    { tceHelloWorld :: route
        :- "hello"
        :> QueryParam "name" Text
        :> Verb 'GET 200 '[JSON] Text
    , tceError :: route
        :- "error"
        :> Verb 'GET 200 '[JSON] ()
    } deriving (Generic)

-- | API type specification.
type TrialChainAPI =
    "api" :> ToServant TrialChainEndpoints AsApi

trialChainAPI :: Proxy TrialChainAPI
trialChainAPI = Proxy
