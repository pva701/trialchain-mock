module TrialChain.Web.Handlers
       ( trialChainHandlers
       ) where

import Universum

import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import TrialChain.App
import TrialChain.Web.API

type TrialChainHandlers m = ToServant TrialChainEndpoints (AsServerT m)

-- | Server handler implementation for TrialChain API.
trialChainHandlers :: forall m . WithError m => TrialChainHandlers m
trialChainHandlers = genericServerT @TrialChainEndpoints @m trialChainEndpoints

trialChainEndpoints :: WithError m => TrialChainEndpoints (AsServerT m)
trialChainEndpoints = TrialChainEndpoints
    { tceHelloWorld = getHelloWorld
    , tceError      = getError
    }

getHelloWorld :: WithError m => Maybe Text -> m Text
getHelloWorld mWho = pure $ "Hello, " <> fromMaybe "Anonymous" mWho

getError :: WithError m => m ()
getError = throwError $ InternalError "This method just returns error constantly"
