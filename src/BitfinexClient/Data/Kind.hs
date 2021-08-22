module BitfinexClient.Data.Kind
  ( Method (..),
    CurrencyRelation (..),
  )
where

data Method
  = MarketAveragePrice
  | OrderTest

data CurrencyRelation
  = Base
  | Quote
