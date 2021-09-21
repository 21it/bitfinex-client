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
    cancelOrderById,
    cancelOrderByClientId,
    cancelOrderByGroupId,
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
  OrderId ->
  SubmitOrder.Request ->
  ExceptT Error m (Order 'Remote)
verifyOrder env id0 req = do
  remOrd <- getOrder env id0
  let locOrd =
        Order
          { orderId = id0,
            orderGroupId = SubmitOrder.groupId opts,
            orderClientId =
              SubmitOrder.clientId opts <|> orderClientId remOrd,
            orderAction = SubmitOrder.action req,
            orderAmount = SubmitOrder.amount req,
            orderSymbol = SubmitOrder.symbol req,
            orderRate = SubmitOrder.rate req,
            orderStatus = orderStatus remOrd
          }
  if remOrd == locOrd
    then pure remOrd
    else throwE $ ErrorUnverifiedOrder (coerce locOrd) remOrd
  where
    opts = SubmitOrder.options req

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
  order :: Order 'Remote <-
    Generic.prv (Generic.Rpc :: Generic.Rpc 'SubmitOrder) env req
  verifyOrder env (orderId order) req
  where
    req =
      SubmitOrder.Request
        { SubmitOrder.action = act,
          SubmitOrder.amount = amt,
          SubmitOrder.symbol = sym,
          SubmitOrder.rate = rate,
          SubmitOrder.options = opts
        }

cancelOrderMulti ::
  MonadIO m =>
  Env ->
  CancelOrderMulti.Request ->
  ExceptT Error m (Map OrderId (Order 'Remote))
cancelOrderMulti =
  Generic.prv
    (Generic.Rpc :: Generic.Rpc 'CancelOrderMulti)

cancelOrderById ::
  MonadIO m =>
  Env ->
  OrderId ->
  ExceptT Error m (Order 'Remote)
cancelOrderById env id0 = do
  mOrder <-
    Map.lookup id0
      <$> cancelOrderMulti
        env
        ( CancelOrderMulti.ByOrderId $ Set.singleton id0
        )
  except $
    maybeToRight (ErrorMissingOrder id0) mOrder

cancelOrderByClientId ::
  MonadIO m =>
  Env ->
  OrderClientId ->
  UTCTime ->
  ExceptT Error m (Map OrderId (Order 'Remote))
cancelOrderByClientId env cid utc =
  cancelOrderMulti env . CancelOrderMulti.ByOrderClientId $
    Set.singleton (cid, utc)

cancelOrderByGroupId ::
  MonadIO m =>
  Env ->
  OrderGroupId ->
  ExceptT Error m (Map OrderId (Order 'Remote))
cancelOrderByGroupId env gid = do
  cancelOrderMulti env . CancelOrderMulti.ByOrderGroupId $
    Set.singleton gid

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
