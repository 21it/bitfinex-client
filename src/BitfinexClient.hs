module BitfinexClient
  ( marketAveragePrice,
    feeSummary,
    submitOrder,
    retrieveOrders,
    ordersHistory,
  )
where

import qualified BitfinexClient.Data.FeeSummary as FeeSummary
import qualified BitfinexClient.Data.GetOrders as GetOrders
import qualified BitfinexClient.Data.MarketAveragePrice as MarketAveragePrice
import qualified BitfinexClient.Data.SubmitOrder as SubmitOrder
import BitfinexClient.Import
import qualified BitfinexClient.Rpc.Generic as GenericRpc

marketAveragePrice ::
  MonadIO m =>
  CurrencyPair ->
  Rational ->
  ExceptT Error m ExchangeRate
marketAveragePrice symbol amount =
  GenericRpc.pub
    (Rpc :: Rpc 'MarketAveragePrice)
    [ SomeQueryParam "symbol" symbol,
      SomeQueryParam "amount" amount
    ]
    MarketAveragePrice.Request
      { MarketAveragePrice.symbol = symbol,
        MarketAveragePrice.amount = amount
      }

feeSummary ::
  MonadIO m =>
  Env ->
  ExceptT Error m FeeSummary.Response
feeSummary env =
  GenericRpc.prv
    (Rpc :: Rpc 'FeeSummary)
    env
    (mempty :: Map Int Int)

submitOrder ::
  MonadIO m =>
  Env ->
  ExchangeRate ->
  Rational ->
  Set OrderFlag ->
  ExceptT Error m Order
submitOrder env rate amount flags =
  GenericRpc.prv
    (Rpc :: Rpc 'SubmitOrder)
    env
    SubmitOrder.Request
      { SubmitOrder.rate = rate,
        SubmitOrder.amount = amount,
        SubmitOrder.flags = flags
      }

retrieveOrders ::
  MonadIO m =>
  Env ->
  CurrencyPair ->
  Set OrderId ->
  ExceptT Error m (Map OrderId Order)
retrieveOrders env pair ids =
  GenericRpc.prv
    (Rpc :: Rpc 'RetrieveOrders)
    env
    GetOrders.Request
      { GetOrders.currencyPair = pair,
        GetOrders.orderIds = ids
      }

ordersHistory ::
  MonadIO m =>
  Env ->
  CurrencyPair ->
  Set OrderId ->
  ExceptT Error m (Map OrderId Order)
ordersHistory env pair ids =
  GenericRpc.prv
    (Rpc :: Rpc 'OrdersHistory)
    env
    GetOrders.Request
      { GetOrders.currencyPair = pair,
        GetOrders.orderIds = ids
      }
