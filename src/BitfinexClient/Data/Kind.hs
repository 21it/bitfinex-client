module BitfinexClient.Data.Kind
  ( Method (..),
    CurrencyRelation (..),
  )
where

import BitfinexClient.Import.External

data Method
  = MarketAveragePrice
  | SubmitOrder
  | RetrieveOrders
  | FeeSummary
  deriving (Show)

data CurrencyRelation
  = Base
  | Quote
