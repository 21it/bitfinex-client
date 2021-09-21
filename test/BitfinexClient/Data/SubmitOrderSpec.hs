{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.SubmitOrderSpec
  ( spec,
  )
where

import qualified BitfinexClient.Data.SubmitOrder as SubmitOrder
import BitfinexClient.Import
import BitfinexClient.TestEnv
import qualified Data.Aeson as A
import Test.Hspec

spec :: Spec
spec =
  describe "ToJSON" $ do
    it "Request" $ do
      x <- withAdaBtc $ \amt sym -> do
        rate <- except . newExchangeRate $ 1 % 1234
        let opts = SubmitOrder.optsPostOnly
            req = SubmitOrder.Request Buy amt sym rate opts
        lift $
          A.encode req
            `shouldBe` "{\"amount\":\"2\",\"flags\":4096,\"symbol\":\"tADABTC\",\"price\":\"0.000810372771\",\"type\":\"EXCHANGE LIMIT\"}"
      x `shouldSatisfy` isRight
