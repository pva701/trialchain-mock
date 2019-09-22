module TrialChain.UtilSpec (spec) where

import Universum

import Data.Aeson (encode, eitherDecode)

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck.Property ((===))

import TrialChain.Util

spec :: Spec
spec = describe "Hex tests" $ do
    it "Hex roundtrip 1" $
        second prettyHex (readHex "0123456789abcdef") === Right "0123456789abcdef"

    it "Hex roundtrip 2" $
        second prettyHex (readHex "0123456789ABCDEF") === Right "0123456789abcdef"

    it "Hex JSON roundtrip 1" $
        eitherDecode @(Hex ()) (encode @Text "0123456789abcdef") === second Hex (readHex "0123456789abcdef")
