module Main where

import Universum

import TrialChain.App
import TrialChain.Web

main :: IO ()
main = runApp Env runServer
