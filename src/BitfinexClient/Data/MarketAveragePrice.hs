{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.MarketAveragePrice
  ( Request (..),
  )
where

import BitfinexClient.Import

data Request
  = Request
      { action :: ExchangeAction,
        amount :: MoneyAmount,
        symbol :: CurrencyPair
      }
  deriving (Eq, Ord, Show)
