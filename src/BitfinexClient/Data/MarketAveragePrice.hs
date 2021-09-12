module BitfinexClient.Data.MarketAveragePrice
  ( Request (..),
  )
where

import BitfinexClient.Import
import Data.Aeson.Lens

data Request
  = Request
      { action :: ExchangeAction,
        amount :: MoneyAmount,
        symbol :: CurrencyPair
      }
  deriving (Eq, Ord, Show)

instance FromRpc 'MarketAveragePrice Request ExchangeRate where
  fromRpc req res@(RawResponse raw) = do
    price <-
      failBecause "ExchangeRate is missing" $
        raw ^? nth 0 . _Number
    newExchangeRate
      (action req)
      (toRational price)
      (coerce $ currencyPairBase currencyPair)
      (coerce $ currencyPairQuote currencyPair)
    where
      currencyPair =
        symbol req
      failBecause err =
        maybeToRight $ fromRpcError SubmitOrder res err
