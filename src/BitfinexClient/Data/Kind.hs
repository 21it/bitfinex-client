module BitfinexClient.Data.Kind
  ( Method (..),
    CurrencyRelation (..),
    MarketRelation (..),
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

data MarketRelation
  = Maker
  | Taker
