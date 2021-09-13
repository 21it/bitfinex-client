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
  it "marketAveragePrice succeeds" . const $ do
    x <- withAdaBtc $ \amt pair -> do
      buy <- Bitfinex.marketAveragePrice Buy amt pair
      sell <- Bitfinex.marketAveragePrice Sell amt pair
      liftIO $ buy `shouldSatisfy` (> sell)
    x `shouldSatisfy` isRight
  it "marketAveragePrice fails" . const $ do
    x <- runExceptT $ do
      amt <- except $ newMoneyAmount 2
      pair <- except $ newCurrencyPair "BTC" "ADA"
      Bitfinex.marketAveragePrice Buy amt pair
    x `shouldSatisfy` isLeft
  it "unOrderFlagSet works" . const $
    unOrderFlagSet [Hidden, PostOnly]
      `shouldBe` OrderFlagAcc 4160
  it "feeSummary succeeds" $ \env -> do
    x <- runExceptT $ Bitfinex.feeSummary env
    x `shouldSatisfy` isRight
  it "submitOrder succeeds" $ \env -> do
    x <- withAdaBtc $ \amt sym -> do
      tweak <- except . newExchangeRate $ 995 % 1000
      rate <- Bitfinex.marketAveragePrice Buy amt sym
      Bitfinex.submitOrder env Buy amt sym (tweak * rate) [PostOnly]
    x `shouldSatisfy` isRight
  it "retrieveOrders succeeds" $ \env -> do
    x <- withAdaBtc . const $ \pair ->
      Bitfinex.retrieveOrders env pair []
    x `shouldSatisfy` isRight
  it "ordersHistory succeeds" $ \env -> do
    x <- withAdaBtc . const $ \pair ->
      Bitfinex.ordersHistory env pair []
    x `shouldSatisfy` isRight
  it "getOrders succeeds" $ \env -> do
    x <- withAdaBtc . const $ \pair ->
      Bitfinex.getOrders env pair []
    x `shouldSatisfy` isRight
  it "getOrder succeeds" $ \env -> do
    x <- withAdaBtc . const $ \pair ->
      Bitfinex.getOrder env pair $ OrderId 0
    x `shouldSatisfy` isRight

withAdaBtc ::
  Monad m =>
  (MoneyAmount -> CurrencyPair -> ExceptT Error m a) ->
  m (Either Error a)
withAdaBtc this = runExceptT $ do
  amt <- except $ newMoneyAmount 2
  pair <- except $ newCurrencyPair "ADA" "BTC"
  this amt pair
