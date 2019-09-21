module TrialChain.App.RIO
       ( -- * Application monad
         App (..)
       , runApp
       ) where

import Universum

import Control.Exception (throwIO)
import Control.Monad.Except (MonadError (..))
import UnliftIO (MonadUnliftIO)

import TrialChain.App.Env (Env)
import TrialChain.App.Error (AppError, AppException (..))

-- | Main application monad.
newtype App a = App
    { unApp :: ReaderT Env IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadUnliftIO)

instance MonadError AppError App where
    throwError = liftIO . throwIO . AppException
    {-# INLINE throwError #-}

    catchError action handler = App $ ReaderT $ \env -> do
        let ioAction = runApp env action
        ioAction `catch` \(AppException e) -> runApp env $ handler e
    {-# INLINE catchError #-}

runApp :: Env -> App a -> IO a
runApp env = usingReaderT env . unApp