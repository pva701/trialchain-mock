module TrialChain.Util
       ( Hex (..)
       , UnitHex
       , JsonViaBuildable (..)
       , defaultOptions

       -- * For tests
       , prettyHex
       , readHex

       -- * For debug purposes
       , unsafeReadHex
       ) where

import Universum

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.Options as A (defaultOptions)
import qualified Data.Aeson.Types as A hiding (defaultOptions)
import qualified Data.ByteString.Base16 as B16
import Fmt (Buildable (..), pretty)

-- | ByteString which is printed to JSON and parsed from
-- as a hexadecimal value.
-- Phantom type serves the same purposes as for Hash.
newtype Hex a = Hex {unHex :: ByteString}
    deriving (Eq, Ord, Show)

type UnitHex = Hex ()

instance ToJSON (Hex a) where
    toJSON = toJSON . prettyHex . unHex

instance FromJSON (Hex a) where
    parseJSON = either fail (pure . Hex) . readHex <=< parseJSON @Text

instance Buildable (Hex a) where
    build = pretty . prettyHex . unHex

-- | Print a hexadecimal string.
prettyHex :: ByteString -> Text
prettyHex = decodeUtf8 . B16.encode

-- | Parse a hexadecimal string safely.
readHex :: Text -> Either String ByteString
readHex t = do
    let (res, rest) = B16.decode (encodeUtf8 t)
    if not (null rest) then Left "invalid hex string"
    else Right res

-- | Parse a hexadecimal string. Call error in case of failure.
unsafeReadHex :: Text -> ByteString
unsafeReadHex = either (error . fromString) id . readHex

newtype JsonViaBuildable a = JsonViaBuildable a

instance Buildable a => ToJSON (JsonViaBuildable a) where
    toJSON (JsonViaBuildable x) = toJSON (pretty @_ @Text x)

defaultOptions :: A.Options
defaultOptions = A.defaultOptions {A.omitNothingFields = True}