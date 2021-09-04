module BitfinexClientSpec
  ( spec,
  )
where

import qualified BitfinexClient as Bitfinex
import BitfinexClient.Import
import Test.Hspec

spec :: Spec
spec = before newEnv $ do
  it "MarketAveragePrice succeeds" . const $ do
    x <- runExceptT $ Bitfinex.marketAveragePrice (CurrencyPair "ADA" "BTC") 1
    x `shouldSatisfy` isRight
  it "MarketAveragePrice reversed fails" . const $ do
    x <- runExceptT $ Bitfinex.marketAveragePrice (CurrencyPair "BTC" "ADA") 1
    x `shouldSatisfy` isLeft
  it "MarketAveragePrice identity fails" . const $ do
    x <- runExceptT $ Bitfinex.marketAveragePrice (CurrencyPair "BTC" "BTC") 1
    x `shouldSatisfy` isLeft
  it "unOrderFlagSet works" . const $
    unOrderFlagSet [Hidden, PostOnly]
      `shouldBe` OrderFlagAcc 4160
  it "SubmitOrder succeeds" $ \env -> do
    x <- runExceptT $ do
      rate <- Bitfinex.marketAveragePrice (CurrencyPair "ADA" "BTC") 1
      print rate
      Bitfinex.submitOrder env rate 2 [PostOnly]
    print x
    x `shouldSatisfy` isRight
  it "FeeSummary succeeds" $ \env -> do
    x <- runExceptT $ Bitfinex.feeSummary env
    x `shouldSatisfy` isRight
