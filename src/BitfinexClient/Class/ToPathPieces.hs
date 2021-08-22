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
  toPathPiece Rpc = ["calc", "trade", "avg"]
