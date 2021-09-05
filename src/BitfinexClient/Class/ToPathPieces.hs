module BitfinexClient.Class.ToPathPieces
  ( ToPathPieces (..),
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Import.External

class ToPathPieces a where
  toPathPiece :: a -> [Text]

instance ToPathPieces (Rpc 'MarketAveragePrice) where
  toPathPiece Rpc = ["v2", "calc", "trade", "avg"]

instance ToPathPieces (Rpc 'FeeSummary) where
  toPathPiece Rpc = ["v2", "auth", "r", "summary"]

instance ToPathPieces (Rpc 'SubmitOrder) where
  toPathPiece Rpc = ["v2", "auth", "w", "order", "submit"]

instance ToPathPieces (Rpc 'RetrieveOrders) where
  toPathPiece Rpc = ["v2", "auth", "r", "orders"]

instance ToPathPieces (Rpc 'OrdersHistory) where
  toPathPiece Rpc = ["v2", "auth", "r", "orders", "hist"]
