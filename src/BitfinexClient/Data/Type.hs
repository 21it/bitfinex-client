module BitfinexClient.Data.Type
  ( LogFormat (..),
    Rpc (..),
    Error (..),
    PrvKey (..),
    ApiKey (..),
    RequestMethod (..),
    BaseUrl (..),
    RawResponse (..),
    Nonce (..),
    PosRat,
    unPosRat,
    newPosRat,
  )
where

import BitfinexClient.Class.ToRequestParam
import BitfinexClient.Data.Kind
import BitfinexClient.Import.External
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Network.HTTP.Client as Web
import qualified Prelude

data LogFormat
  = Bracket
  | Json
  deriving (Eq, Ord, Show, Read)

data Rpc (method :: Method)
  = Rpc

data Error
  = ErrorWebException HttpException
  | ErrorWebPub Web.Request (Web.Response ByteString)
  | ErrorWebPrv ByteString Web.Request (Web.Response ByteString)
  | ErrorFromRpc Text
  | ErrorSmartCon Text
  deriving (Show)

newtype PrvKey
  = PrvKey BS.ByteString
  deriving newtype (Eq, Ord, IsString)

instance Prelude.Show PrvKey where
  show = const "SECRET"

newtype ApiKey
  = ApiKey BS.ByteString
  deriving newtype (Eq, Ord, IsString)

instance Prelude.Show ApiKey where
  show = const "SECRET"

data RequestMethod
  = GET
  | POST
  deriving (Eq, Ord, Show)

newtype BaseUrl
  = BaseUrl Text
  deriving newtype (Eq, Ord, Show, IsString)

newtype RawResponse
  = RawResponse ByteString
  deriving newtype (Eq, Ord)

instance Show RawResponse where
  show x =
    case decodeUtf8' bs of
      Left {} -> "ByteString RawResponse" <> show (BS.unpack bs)
      Right res -> "Text RawResponse " <> T.unpack res
    where
      bs = BL.toStrict $ coerce x

newtype Nonce
  = Nonce Integer
  deriving newtype (Eq, Ord, Show)

newtype PosRat
  = PosRat {unPosRat :: Rational}
  deriving newtype (Eq, Ord, Show, Num, ToRequestParam)

newPosRat :: Rational -> Either Error PosRat
newPosRat x
  | x > 0 = Right $ PosRat x
  | otherwise =
    Left . ErrorSmartCon $ "PosRat should be positive, but got " <> show x
