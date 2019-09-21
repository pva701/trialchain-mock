module Main where

import Universum

import TrialChain.App
import TrialChain.Web

main :: IO ()
main = flip runApp runServer =<< newEmptyEnv
