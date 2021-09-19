{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClientSpec
  ( spec,
  )
where

import qualified BitfinexClient as Bitfinex
import qualified BitfinexClient.Data.GetOrders as GetOrders
import qualified BitfinexClient.Data.SubmitOrder as SubmitOrder
import BitfinexClient.Import
import qualified Data.Aeson as A
import Test.Hspec

spec :: Spec
spec = before newEnv $ do
  it "newCurrencyPair succeeds" . const $
    newCurrencyPair "ADA" "BTC" `shouldSatisfy` isRight
  it "newCurrencyPair fails" . const $
    newCurrencyPair "BTC" "BTC" `shouldSatisfy` isLeft
  it "marketAveragePrice succeeds" . const $ do
    x <- withAdaBtc $ \amt sym -> do
      buy <- Bitfinex.marketAveragePrice Buy amt sym
      sell <- Bitfinex.marketAveragePrice Sell amt sym
      liftIO $ buy `shouldSatisfy` (> sell)
    x `shouldSatisfy` isRight
  it "marketAveragePrice fails" . const $ do
    x <- runExceptT $ do
      amt <- except $ newMoneyAmount 2
      sym <- except $ newCurrencyPair "BTC" "ADA"
      Bitfinex.marketAveragePrice Buy amt sym
    x `shouldSatisfy` isLeft
  it "unOrderFlagSet works" . const $
    unOrderFlagSet [Hidden, PostOnly]
      `shouldBe` OrderFlagAcc 4160
  it "feeSummary succeeds" $ \env -> do
    x <- runExceptT $ Bitfinex.feeSummary env
    x `shouldSatisfy` isRight
  it "ToJSON SubmitOrder" . const $ do
    x <- withAdaBtc $ \amt sym -> do
      rate <- except . newExchangeRate $ 1 % 1234
      let opts = SubmitOrder.optsPostOnly
          req = SubmitOrder.Request Buy amt sym rate opts
      lift $ A.encode req `shouldBe` "{\"amount\":\"2\",\"flags\":4096,\"symbol\":\"tADABTC\",\"gid\":null,\"price\":\"0.000810372771\",\"type\":\"EXCHANGE LIMIT\",\"cid\":null}"
    x `shouldSatisfy` isRight
  it "submitOrder succeeds" $ \env -> do
    x <- withAdaBtc $ \amt sym -> do
      --tweak <- except . newExchangeRate $ 995 % 1000
      tweak <- except . newExchangeRate $ 1000 % 1000
      rate <- Bitfinex.marketAveragePrice Buy amt sym
      let opts = SubmitOrder.optsPostOnly
      Bitfinex.submitOrder env Buy amt sym (tweak * rate) opts
    print x
    x `shouldSatisfy` isRight
  it "retrieveOrders succeeds" $ \env -> do
    x <- withAdaBtc . const $ \sym ->
      Bitfinex.retrieveOrders env $ GetOrders.optsSym sym
    x `shouldSatisfy` isRight
  it "ordersHistory succeeds" $ \env -> do
    x <- withAdaBtc . const $ \sym ->
      Bitfinex.ordersHistory env $ GetOrders.optsSym sym
    x `shouldSatisfy` isRight
  it "getOrders succeeds" $ \env -> do
    x <- withAdaBtc . const $ \sym ->
      Bitfinex.getOrders env $ GetOrders.optsSym sym
    x `shouldSatisfy` isRight
  it "getOrder succeeds" $ \env -> do
    x <-
      withAdaBtc . const . const
        $ Bitfinex.getOrder env
        $ OrderId 0
    x `shouldSatisfy` isRight

withAdaBtc ::
  Monad m =>
  (MoneyAmount -> CurrencyPair -> ExceptT Error m a) ->
  m (Either Error a)
withAdaBtc this = runExceptT $ do
  amt <- except $ newMoneyAmount 2
  sym <- except $ newCurrencyPair "ADA" "BTC"
  this amt sym
