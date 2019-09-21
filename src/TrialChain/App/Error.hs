{-# LANGUAGE DeriveAnyClass #-}

module TrialChain.App.Error
       ( AppError (..)
       , AppErrorType (..)
       , toAppError
       , AppException (..)
       , WithError
       , throwError
       , toServantError
       ) where

import Universum

import Control.Monad.Except (MonadError)
import GHC.Stack (SrcLoc (SrcLoc, srcLocModule, srcLocStartLine))
import Servant.Server (err404, err500, errBody)

import qualified Control.Monad.Except as E (throwError)
import qualified Servant.Server as Servant (ServerError)

-- | Type alias for errors.
type WithError m = (MonadError AppError m, HasCallStack)

-- | Specialized version of 'E.throwError'
throwError :: WithError m => AppErrorType -> m a
throwError = E.throwError . toAppError
{-# INLINE throwError #-}

newtype SourcePosition = SourcePosition Text
    deriving newtype (Show, Eq)

-- | Display 'CallStack' as 'SourcePosition' in a format: @Module.function#line_number@.
toSourcePosition :: CallStack -> SourcePosition
toSourcePosition cs = SourcePosition showCallStack
  where
    showCallStack :: Text
    showCallStack = case getCallStack cs of
        []                             -> "<unknown loc>"
        [(name, loc)]                  -> showLoc name loc
        (_, loc) : (callerName, _) : _ -> showLoc callerName loc

    showLoc :: String -> SrcLoc -> Text
    showLoc name SrcLoc{..} =
        toText srcLocModule <> "." <> toText name <> "#" <> show srcLocStartLine

{- | Exception wrapper around 'AppError'. Useful when you need to throw/catch
'AppError' as 'Exception'.
-}
newtype AppException = AppException
    { unAppException :: AppError
    } deriving (Show)
      deriving anyclass (Exception)

-- | 'AppErrorType' with the corresponding 'CallStack'.
data AppError = AppError
    { appErrorCallStack :: !SourcePosition
    , appErrorType      :: !AppErrorType
    } deriving (Show, Eq)

-- | App errors type.
data AppErrorType
    -- | General not found.
    = NotFound
    | InternalError Text
    deriving (Show, Eq)

toAppError :: HasCallStack => AppErrorType -> AppError
toAppError = AppError (toSourcePosition callStack)

-- | Map 'AppErrorType' into a HTTP error code.
toServantError :: AppError -> Servant.ServerError
toServantError (AppError _callStack errorType) =
    case errorType of
        NotFound             -> err404
        InternalError reason -> err500 { errBody = encodeUtf8 reason }