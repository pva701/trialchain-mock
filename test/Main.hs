module Main where

import Universum

import Test.Hspec (hspec)

import qualified Spec

main :: IO ()
main = hspec Spec.spec
