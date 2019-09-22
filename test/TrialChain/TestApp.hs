module TrialChain.TestApp
       ( runTestApp
       , hoistTestApp
       , TestApp
       ) where

import Universum

import Test.QuickCheck.Monadic (PropertyM (..), monadicIO)
import Test.QuickCheck.Property (Property)
import Test.QuickCheck (Testable)

import TrialChain.App
import TrialChain.Consensus

type TestApp = PropertyM App

runTestApp :: Testable a => Balances -> TestApp a -> Property
runTestApp b action = monadicIO $ hoistTestApp b action

hoistTestApp :: Testable a => Balances -> TestApp a -> PropertyM IO a
hoistTestApp balances (MkPropertyM action) = do
    env <- liftIO (newEnv balances)
    MkPropertyM $ \callIO -> runApp env <$> action (fmap liftIO . callIO)