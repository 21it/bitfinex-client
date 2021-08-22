module BitfinexClient.Class.ToQueryParam
  ( ToQueryParam (..),
    SomeQueryParam (..),
    unQueryParam,
  )
where

import BitfinexClient.Data.Type
import BitfinexClient.Import.External
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

class ToQueryParam a where
  toQueryParam :: a -> Maybe BS.ByteString

data SomeQueryParam
  = forall a. ToQueryParam a => SomeQueryParam BS.ByteString a

instance ToQueryParam CurrencyPair where
  toQueryParam x =
    Just $
      "t"
        <> encodeUtf8 (coerce $ currencyPairBase x :: Text)
        <> encodeUtf8 (coerce $ currencyPairQuote x :: Text)

instance ToQueryParam Rational where
  toQueryParam x =
    Just . C8.pack $
      --
      -- TODO : what precision should be here?
      --
      showFixed True (fromRational x :: Fixed E12)

unQueryParam :: SomeQueryParam -> (BS.ByteString, Maybe BS.ByteString)
unQueryParam (SomeQueryParam name x) = (name, toQueryParam x)
