module TrialChain.Web.Server
       ( runServer
       ) where

import Universum

import qualified Network.Wai.Handler.Warp as Warp
import Servant (Application, Handler, Server, hoistServer, serve, throwError)
import UnliftIO (UnliftIO (..))
import qualified UnliftIO as UIO

import TrialChain.App
import TrialChain.Web.API
import TrialChain.Web.Handlers

-- | Helper for running a Warp server on a given listen port in
-- arbitrary @MonadIO@.
serveWeb :: MonadIO m => Int -> Application -> m a
serveWeb port app = do
    let settings = Warp.defaultSettings
                 & Warp.setPort port
    liftIO $ Warp.runSettings settings app
    return $ error "Server terminated early"

-- | Makes the @Server@ for TrialChain API, given the natural
-- transformation from the working monad to @Handler@.
trialChainServer
  :: forall env m . WorkMode env m
  => (forall a. m a -> Handler a)
  -> Server TrialChainAPI
trialChainServer nat = hoistServer trialChainAPI nat trialChainHandlers

-- | Natural transformation which uses @UnliftIO@ to convert
-- an arbitrary monadic handler which throws @AppError@s
-- to a regular @Handler@.
convertTrialChainHandler :: UnliftIO m -> m a -> Handler a
convertTrialChainHandler (UnliftIO unlift) action =
    liftIO (unlift action)
    `catch` throwServant
    `catchAny` (throwServant . AppException . toAppError . InternalError . show)
    where
        throwServant = Servant.throwError . toServantError . unAppException

-- | Runs the web server which serves TrialChain API.
runServer :: WorkMode env m => m ()
runServer = do
    unlift <- UIO.askUnliftIO
    let apiServer = trialChainServer $ convertTrialChainHandler unlift
    serveWeb 8080 $ serve trialChainAPI apiServer