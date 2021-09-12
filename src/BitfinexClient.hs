module BitfinexClient
  ( marketAveragePrice,
    feeSummary,
    submitOrder,
    retrieveOrders,
    ordersHistory,
    getOrders,
    getOrder,
    module X,
  )
where

import qualified BitfinexClient.Data.FeeSummary as FeeSummary
import qualified BitfinexClient.Data.GetOrders as GetOrders
import qualified BitfinexClient.Data.MarketAveragePrice as MarketAveragePrice
import qualified BitfinexClient.Data.SubmitOrder as SubmitOrder
import BitfinexClient.Import
import qualified BitfinexClient.Import.Internal as X
import qualified BitfinexClient.Rpc.Generic as Generic
import qualified Data.Map as Map
import qualified Data.Set as Set

marketAveragePrice ::
  MonadIO m =>
  ExchangeAction ->
  MoneyAmount ->
  CurrencyPair ->
  ExceptT Error m ExchangeRate
marketAveragePrice act amt sym =
  Generic.pub
    (Generic.Rpc :: Generic.Rpc 'MarketAveragePrice)
    [ SomeQueryParam "amount" $ newRawAmt act amt,
      SomeQueryParam "symbol" sym
    ]
    MarketAveragePrice.Request
      { MarketAveragePrice.action = act,
        MarketAveragePrice.amount = amt,
        MarketAveragePrice.symbol = sym
      }

feeSummary ::
  MonadIO m =>
  Env ->
  ExceptT Error m FeeSummary.Response
feeSummary env =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'FeeSummary)
    env
    (mempty :: Map Int Int)

submitOrder ::
  MonadIO m =>
  Env ->
  ExchangeRate ->
  MoneyAmount ->
  Set OrderFlag ->
  ExceptT Error m Order
submitOrder env rate amt flags =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'SubmitOrder)
    env
    SubmitOrder.Request
      { SubmitOrder.rate = rate,
        SubmitOrder.amount = amt,
        SubmitOrder.flags = flags
      }

retrieveOrders ::
  MonadIO m =>
  Env ->
  CurrencyPair ->
  Set OrderId ->
  ExceptT Error m (Map OrderId Order)
retrieveOrders env pair ids =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'RetrieveOrders)
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
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'OrdersHistory)
    env
    GetOrders.Request
      { GetOrders.currencyPair = pair,
        GetOrders.orderIds = ids
      }

getOrders ::
  MonadIO m =>
  Env ->
  CurrencyPair ->
  Set OrderId ->
  ExceptT Error m (Map OrderId Order)
getOrders env pair ids = do
  xs0 <- retrieveOrders env pair ids
  xs1 <- ordersHistory env pair ids
  pure $ xs1 <> xs0

getOrder ::
  MonadIO m =>
  Env ->
  CurrencyPair ->
  OrderId ->
  ExceptT Error m (Maybe Order)
getOrder env pair id0 =
  Map.lookup id0
    <$> getOrders env pair (Set.singleton id0)
