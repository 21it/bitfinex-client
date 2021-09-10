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
    x <- withAdaBtc $ \pair ->
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
    x <- withAdaBtc $ \pair -> do
      tweak <- except . newPosRat $ 995 % 1000
      rate <-
        tweakExchangeRate (* tweak)
          <$> Bitfinex.marketAveragePrice pair amt
      Bitfinex.submitOrder env rate amt [PostOnly]
    print x
    x `shouldSatisfy` isRight
  it "RetrieveOrders succeeds" $ \env -> do
    x <- withAdaBtc $ \pair ->
      Bitfinex.retrieveOrders env pair []
    print x
    x `shouldSatisfy` isRight
  it "OrdersHistory succeeds" $ \env -> do
    x <- withAdaBtc $ \pair ->
      Bitfinex.ordersHistory env pair []
    print x
    x `shouldSatisfy` isRight
  it "GetOrders succeeds" $ \env -> do
    x <- withAdaBtc $ \pair ->
      Bitfinex.getOrders env pair []
    print x
    x `shouldSatisfy` isRight
  it "GetOrder succeeds" $ \env -> do
    x <- withAdaBtc $ \pair ->
      Bitfinex.getOrder env pair $ OrderId 0
    print x
    x `shouldSatisfy` isRight

withAdaBtc ::
  Monad m =>
  (CurrencyPair -> ExceptT Error m a) ->
  m (Either Error a)
withAdaBtc this = runExceptT $ do
  pair <- except $ newCurrencyPair "ADA" "BTC"
  this pair
