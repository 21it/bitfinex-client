module BitfinexClient.Data.Kind
  ( Method (..),
    CurrencyRelation (..),
  )
where

import BitfinexClient.Import.External

data Method
  = MarketAveragePrice
  | SubmitOrder
  | FeeSummary
  deriving (Show)

data CurrencyRelation
  = Base
  | Quote
