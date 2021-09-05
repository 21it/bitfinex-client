{-# LANGUAGE AllowAmbiguousTypes #-}

module BitfinexClient.Class.ToPathPieces
  ( ToPathPieces (..),
  )
where

import BitfinexClient.Class.ToRequestParam
import qualified BitfinexClient.Data.GetOrders as GetOrders
import BitfinexClient.Data.Kind
import BitfinexClient.Import.External

class ToPathPieces (method :: Method) req where
  toPathPieces :: req -> [Text]

instance ToPathPieces 'MarketAveragePrice req where
  toPathPieces =
    const ["v2", "calc", "trade", "avg"]

instance ToPathPieces 'FeeSummary req where
  toPathPieces =
    const ["v2", "auth", "r", "summary"]

instance ToPathPieces 'SubmitOrder req where
  toPathPieces =
    const ["v2", "auth", "w", "order", "submit"]

instance ToPathPieces 'RetrieveOrders GetOrders.Request where
  toPathPieces req =
    [ "v2",
      "auth",
      "r",
      "orders",
      toBodyParam $ GetOrders.currencyPair req
    ]

instance ToPathPieces 'OrdersHistory GetOrders.Request where
  toPathPieces req =
    [ "v2",
      "auth",
      "r",
      "orders",
      toBodyParam $ GetOrders.currencyPair req,
      "hist"
    ]
