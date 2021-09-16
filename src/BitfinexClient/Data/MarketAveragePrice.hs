{-# OPTIONS_HADDOCK show-extensions #-}

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
  fromRpc _ res@(RawResponse raw) = do
    x <-
      maybeToRight
        (fromRpcError MarketAveragePrice res "ExchangeRate is missing")
        (toRational <$> raw ^? nth 0 . _Number)
    newExchangeRate x
