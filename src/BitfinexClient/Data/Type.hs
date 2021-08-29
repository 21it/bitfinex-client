module BitfinexClient.Data.Type
  ( LogFormat (..),
    CurrencyCode (..),
    CurrencyPair (..),
    Rpc (..),
    Error (..),
    PrvKey (..),
    ApiKey (..),
    RequestMethod (..),
    BaseUrl (..),
    OrderId (..),
    Order (..),
    RawResponse (..),
    Nonce (..),
  )
where

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

newtype CurrencyCode (a :: CurrencyRelation)
  = CurrencyCode Text
  deriving newtype (Eq, Ord, Show, Read, IsString)

data CurrencyPair
  = CurrencyPair
      { currencyPairBase :: CurrencyCode 'Base,
        currencyPairQuote :: CurrencyCode 'Quote
      }
  deriving (Eq, Ord, Show)

data Rpc (method :: Method)
  = Rpc

data Error
  = ErrorWebException HttpException
  | ErrorWebResponse Web.Request (Web.Response ByteString)
  | ErrorFromRpc Text
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

newtype OrderId
  = OrderId Integer
  deriving newtype (Eq, Ord, Show)

data Order
  = Order
      { orderId :: OrderId,
        orderRate :: SomeExchangeRate,
        orderAmount :: Rational
      }
  deriving (Eq, Ord, Show)

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
