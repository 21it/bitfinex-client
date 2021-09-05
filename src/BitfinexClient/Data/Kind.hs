module BitfinexClient.Data.Kind
  ( Method (..),
    CurrencyRelation (..),
  )
where

import BitfinexClient.Import.External

data Method
  = MarketAveragePrice
  | FeeSummary
  | SubmitOrder
  | RetrieveOrders
  | OrdersHistory
  deriving (Show)

data CurrencyRelation
  = Base
  | Quote
