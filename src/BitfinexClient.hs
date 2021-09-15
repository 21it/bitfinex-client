module BitfinexClient
  ( marketAveragePrice,
    feeSummary,
    submitOrder,
    retrieveOrders,
    ordersHistory,
    getOrders,
    getOrder,
    --submitCounterOrder,
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
  ExchangeAction ->
  MoneyAmount ->
  CurrencyPair ->
  ExchangeRate ->
  SubmitOrder.Options ->
  ExceptT Error m Order
submitOrder env act amt sym rate opts =
  --
  -- TODO : verify order???
  --
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'SubmitOrder)
    env
    SubmitOrder.Request
      { SubmitOrder.action = act,
        SubmitOrder.amount = amt,
        SubmitOrder.symbol = sym,
        SubmitOrder.rate = rate,
        SubmitOrder.options = opts
      }

retrieveOrders ::
  MonadIO m =>
  Env ->
  CurrencyPair ->
  Set OrderId ->
  ExceptT Error m (Map OrderId Order)
retrieveOrders env sym ids =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'RetrieveOrders)
    env
    GetOrders.Request
      { GetOrders.currencyPair = sym,
        GetOrders.orderIds = ids
      }

ordersHistory ::
  MonadIO m =>
  Env ->
  CurrencyPair ->
  Set OrderId ->
  ExceptT Error m (Map OrderId Order)
ordersHistory env sym ids =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'OrdersHistory)
    env
    GetOrders.Request
      { GetOrders.currencyPair = sym,
        GetOrders.orderIds = ids
      }

getOrders ::
  MonadIO m =>
  Env ->
  CurrencyPair ->
  Set OrderId ->
  ExceptT Error m (Map OrderId Order)
getOrders env sym ids = do
  xs0 <- retrieveOrders env sym ids
  xs1 <- ordersHistory env sym ids
  pure $ xs1 <> xs0

getOrder ::
  MonadIO m =>
  Env ->
  CurrencyPair ->
  OrderId ->
  ExceptT Error m Order
getOrder env sym id0 = do
  mOrder <-
    Map.lookup id0
      <$> getOrders env sym (Set.singleton id0)
  except $ maybeToRight (ErrorMissingOrder id0) mOrder
--submitCounterOrder ::
--  MonadIO m =>
--  Env ->
--  Order ->
--  FeeRate a ->
--  ProfitRate ->
--  Set OrderFlag ->
--  ExceptT Error m Order
--submitCounterOrder env ord0 fee prof flags = do
--  ord1 <- updateVerifyOrder env ord0
--  if orderStatus ord1 == Executed
--    then submitOrder env Sell amt (orderSymbol ord1) rate flags
--    else
--      throwE . ErrorOrder $
--        "Wrong status, can not submit counter order for "
--          <> show ord1
