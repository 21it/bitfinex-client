module BitfinexClient.Data.MarketAveragePrice
  ( Request (..),
  )
where

import BitfinexClient.Import
import Data.Aeson.Lens

data Request
  = Request
      { symbol :: CurrencyPair,
        amount :: Rational
      }
  deriving (Eq, Ord, Show)

instance FromRpc 'MarketAveragePrice Request ExchangeRate where
  fromRpc Rpc req res@(RawResponse raw) = do
    price <-
      failBecause "ExchangeRate is missing" $
        raw ^? nth 0 . _Number
    newExchangeRate
      (coerce $ currencyPairBase currencyPair)
      (coerce $ currencyPairQuote currencyPair)
      (toRational price)
    where
      currencyPair =
        symbol req
      failBecause err =
        maybeToRight $ fromRpcError SubmitOrder res err
