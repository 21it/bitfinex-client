module BitfinexClientSpec
  ( spec,
  )
where

import qualified BitfinexClient as Bitfinex
import BitfinexClient.Import
import Test.Hspec

spec :: Spec
spec = do
  it "MarketAveragePrice succeeds" $ do
    x <- runExceptT $ Bitfinex.marketAveragePrice (CurrencyPair "ADA" "BTC") 1
    print x
    x `shouldSatisfy` isRight
  it "MarketAveragePrice fails" $ do
    x <- runExceptT $ Bitfinex.marketAveragePrice (CurrencyPair "BTC" "BTC") 1
    print x
    x `shouldSatisfy` isLeft
