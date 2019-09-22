module Main where

import Universum

import Fmt (fmt, jsonMapF)

import TrialChain.App
import TrialChain.Web
import TrialChain.Consensus

main :: IO ()
main = do
    let initBalances = defaultBalances
    putTextLn "Initial balances"
    putTextLn (fmt $ jsonMapF initBalances)
    env <- newEnv (mkBalances initBalances)
    runApp env runServer
