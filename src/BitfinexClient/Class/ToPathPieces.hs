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

instance ToPathPieces (Rpc 'SubmitOrder) where
  toPathPiece Rpc = ["v2", "auth", "w", "order", "submit"]

instance ToPathPieces (Rpc 'FeeSummary) where
  toPathPiece Rpc = ["v2", "auth", "r", "summary"]
