module BitfinexClient
  ( marketAveragePrice,
    submitOrder,
  )
where

import qualified BitfinexClient.Data.MarketAveragePrice as MarketAveragePrice
import qualified BitfinexClient.Data.SubmitOrder as SubmitOrder
import BitfinexClient.Import
import qualified BitfinexClient.Rpc.Generic as GenericRpc

marketAveragePrice ::
  MonadIO m =>
  CurrencyPair ->
  Rational ->
  ExceptT Error m SomeExchangeRate
marketAveragePrice symbol amount =
  GenericRpc.pub
    (Rpc :: Rpc 'MarketAveragePrice)
    MarketAveragePrice.Request
      { MarketAveragePrice.symbol = symbol,
        MarketAveragePrice.amount = amount
      }
    [ SomeQueryParam "symbol" symbol,
      SomeQueryParam "amount" amount
    ]

submitOrder ::
  MonadIO m =>
  SomeExchangeRate ->
  Rational ->
  ExceptT Error m Order
submitOrder rate amount =
  GenericRpc.prv
    (Rpc :: Rpc 'SubmitOrder)
    SubmitOrder.Request
      { SubmitOrder.rate = rate,
        SubmitOrder.amount = amount
      }
