module BitfinexClient.Class.ToPathPieces
  ( ToPathPieces (..),
  )
where

import BitfinexClient.Class.ToRequestParam
import qualified BitfinexClient.Data.GetOrders as GetOrders
import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Import.External

class ToPathPieces (method :: Method) req where
  toPathPieces :: Rpc (method :: Method) -> req -> [Text]

instance ToPathPieces 'MarketAveragePrice a where
  toPathPieces Rpc =
    const ["v2", "calc", "trade", "avg"]

instance ToPathPieces 'FeeSummary a where
  toPathPieces Rpc =
    const ["v2", "auth", "r", "summary"]

instance ToPathPieces 'SubmitOrder a where
  toPathPieces Rpc =
    const ["v2", "auth", "w", "order", "submit"]

instance ToPathPieces 'RetrieveOrders GetOrders.Request where
  toPathPieces Rpc req =
    [ "v2",
      "auth",
      "r",
      "orders",
      toBodyParam $ GetOrders.currencyPair req
    ]

instance ToPathPieces 'OrdersHistory GetOrders.Request where
  toPathPieces Rpc req =
    [ "v2",
      "auth",
      "r",
      "orders",
      toBodyParam $ GetOrders.currencyPair req,
      "hist"
    ]
