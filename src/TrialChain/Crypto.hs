{- Cryptographic primitives. -}

{-# LANGUAGE DerivingVia #-}

module TrialChain.Crypto
       ( Signature (..)
       , Hash
       , toHash
       , Address (..)
       , PublicKey (..)
       , SecretKey
       , toPublicKey
       , sign
       , verifySignature
       ) where

import Universum

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.ByteArray as ByteArray
import Data.Persist (Persist (..))
import qualified Data.Persist as Persist
import Crypto.Hash (Blake2b_256, Digest, hash, digestFromByteString)
import Fmt (Buildable (..), pretty)

import TrialChain.Util

-- | Hash of a data.
-- Phantom type reflects a type of the object that has been hashed.
newtype Hash a = Hash (Digest Blake2b_256)
    deriving (Eq, Ord, Show, Generic, Persist, FromJSON, ToJSON, Buildable)

instance Persist (Digest Blake2b_256) where
    put x = Persist.put (ByteArray.convert x :: ByteString)
    get = do
        bytes <- Persist.getBytes 32
        Persist.eof
        maybe (fail "invalid hash") pure (digestFromByteString bytes)

instance ToJSON (Digest Blake2b_256) where
    toJSON = toJSON . Hex . ByteArray.convert

instance FromJSON (Digest Blake2b_256) where
    parseJSON = maybe (fail "invalid hash") pure . digestFromByteString <=< fmap unHex . parseJSON

instance Buildable (Digest Blake2b_256) where
    build = pretty . prettyHex . Hex . ByteArray.convert

toHash :: Persist a => a -> Hash a
toHash = Hash . hash . Persist.encode

-- | Signature of a data.
-- Phantom type serves the same purposes as for Hash.
newtype Signature a = Signature ByteString
    deriving (Eq, Ord, Show, Generic, Persist)
    deriving (FromJSON, ToJSON, Buildable) via UnitHex

-- | Address is hash of public key.
newtype Address = Address ByteString
    deriving (Eq, Ord, Show, Generic, Persist)
    deriving (FromJSON, ToJSON, Buildable) via UnitHex

-- | Public key.
newtype PublicKey = PublicKey ByteString
    deriving (Eq, Ord, Show, Generic, Persist)
    deriving (FromJSON, ToJSON, Buildable) via UnitHex

-- | Secret key.
newtype SecretKey = SecretKey ByteString
    deriving (Eq, Ord, Show)
    deriving (FromJSON, ToJSON) via UnitHex

-- | Generate public key from secret one.
-- This function is just mock.
toPublicKey :: SecretKey -> PublicKey
toPublicKey _  = PublicKey $ encodeUtf8 @Text "mock_public"

-- | Sign a data using secret key.
-- This function is just mock.
sign :: Persist a => SecretKey -> a -> Signature a
sign _ _ = Signature $ encodeUtf8 @Text "mock_signature"

-- | Verify a signature.
-- This function is just mock always returning true.
verifySignature :: Persist a => a -> Signature a -> PublicKey -> Bool
verifySignature _ _ _ = True