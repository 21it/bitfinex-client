module BitfinexClient.Data.Kind
  ( Method (..),
    CurrencyRelation (..),
  )
where

import BitfinexClient.Import.External

data Method
  = MarketAveragePrice
  | SubmitOrder
  deriving (Show)

data CurrencyRelation
  = Base
  | Quote
