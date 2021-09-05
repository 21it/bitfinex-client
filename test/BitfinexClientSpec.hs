module BitfinexClientSpec
  ( spec,
  )
where

import qualified BitfinexClient as Bitfinex
import BitfinexClient.Import
import Test.Hspec

spec :: Spec
spec = before newEnv $ do
  it "newCurrencyPair succeeds" . const $
    newCurrencyPair "ADA" "BTC" `shouldSatisfy` isRight
  it "newCurrencyPair fails" . const $
    newCurrencyPair "BTC" "BTC" `shouldSatisfy` isLeft
  it "MarketAveragePrice succeeds" . const $ do
    x <- runExceptT $ do
      pair <- except $ newCurrencyPair "ADA" "BTC"
      Bitfinex.marketAveragePrice pair 1
    x `shouldSatisfy` isRight
  it "MarketAveragePrice fails" . const $ do
    x <- runExceptT $ do
      pair <- except $ newCurrencyPair "BTC" "ADA"
      Bitfinex.marketAveragePrice pair 1
    x `shouldSatisfy` isLeft
  it "unOrderFlagSet works" . const $
    unOrderFlagSet [Hidden, PostOnly]
      `shouldBe` OrderFlagAcc 4160
  it "FeeSummary succeeds" $ \env -> do
    x <- runExceptT $ Bitfinex.feeSummary env
    x `shouldSatisfy` isRight
  it "SubmitOrder succeeds" $ \env -> do
    let amt = 2
    x <- runExceptT $ do
      pair <- except $ newCurrencyPair "ADA" "BTC"
      tweak <- except . newPosRat $ 995 % 1000
      rate <-
        tweakExchangeRate (* tweak)
          <$> Bitfinex.marketAveragePrice pair amt
      Bitfinex.submitOrder env rate amt [PostOnly]
    print x
    x `shouldSatisfy` isRight
  it "RetrieveOrders succeeds" $ \env -> do
    x <- runExceptT $ do
      pair <- except $ newCurrencyPair "ADA" "BTC"
      Bitfinex.retrieveOrders env pair []
    print x
    x `shouldSatisfy` isRight
  it "OrdersHistory succeeds" $ \env -> do
    x <- runExceptT $ do
      pair <- except $ newCurrencyPair "ADA" "BTC"
      Bitfinex.ordersHistory env pair []
    x `shouldSatisfy` isRight
