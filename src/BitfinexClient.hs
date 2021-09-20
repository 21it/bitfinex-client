{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient
  ( marketAveragePrice,
    feeSummary,
    retrieveOrders,
    ordersHistory,
    getOrders,
    getOrder,
    verifyOrder,
    submitOrder,
    cancelOrderMulti,
    submitCounterOrder,
    module X,
  )
where

import qualified BitfinexClient.Data.CancelOrderMulti as CancelOrderMulti
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

retrieveOrders ::
  MonadIO m =>
  Env ->
  GetOrders.Options ->
  ExceptT Error m (Map OrderId (Order 'Remote))
retrieveOrders =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'RetrieveOrders)

ordersHistory ::
  MonadIO m =>
  Env ->
  GetOrders.Options ->
  ExceptT Error m (Map OrderId (Order 'Remote))
ordersHistory =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'OrdersHistory)

getOrders ::
  MonadIO m =>
  Env ->
  GetOrders.Options ->
  ExceptT Error m (Map OrderId (Order 'Remote))
getOrders env opts = do
  xs0 <- retrieveOrders env opts
  xs1 <- ordersHistory env opts
  pure $ xs1 <> xs0

getOrder ::
  MonadIO m =>
  Env ->
  OrderId ->
  ExceptT Error m (Order 'Remote)
getOrder env id0 = do
  mOrder <-
    Map.lookup id0
      <$> getOrders env (GetOrders.optsIds $ Set.singleton id0)
  except $ maybeToRight (ErrorMissingOrder id0) mOrder

verifyOrder ::
  MonadIO m =>
  Env ->
  Order 'Local ->
  ExceptT Error m (Order 'Remote)
verifyOrder env locOrd = do
  remOrd <-
    getOrder env $ orderId locOrd
  if remOrd == locOrd {orderStatus = orderStatus remOrd}
    then pure remOrd
    else throwE $ ErrorUnverifiedOrder locOrd remOrd

submitOrder ::
  MonadIO m =>
  Env ->
  ExchangeAction ->
  MoneyAmount ->
  CurrencyPair ->
  ExchangeRate ->
  SubmitOrder.Options ->
  ExceptT Error m (Order 'Remote)
submitOrder env act amt sym rate opts = do
  order <-
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
  verifyOrder env order

cancelOrderMulti ::
  MonadIO m =>
  Env ->
  CancelOrderMulti.Request ->
  ExceptT Error m (Map OrderId (Order 'Remote))
cancelOrderMulti =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'CancelOrderMulti)

submitCounterOrder ::
  MonadIO m =>
  Env ->
  OrderId ->
  FeeRate a ->
  ProfitRate ->
  SubmitOrder.Options ->
  ExceptT Error m (Order 'Remote)
submitCounterOrder env id0 feeRate profRate opts = do
  order <- getOrder env id0
  amtRate <- except $ 1 `subPosRat` coerce feeRate
  let amt = orderAmount order * coerce amtRate
  let rate = orderRate order * (1 + 2 * coerce feeRate + coerce profRate)
  if orderStatus order == Executed
    then submitOrder env Sell amt (orderSymbol order) rate opts
    else throwE $ ErrorOrderStatus order
