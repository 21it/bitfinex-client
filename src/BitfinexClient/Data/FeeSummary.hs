{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.FeeSummary
  ( Response (..),
  )
where

import BitfinexClient.Import
import Data.Aeson.Lens

data Response
  = Response
      { makerCrypto2CryptoFee :: FeeRate 'Maker,
        makerCrypto2StableFee :: FeeRate 'Maker,
        makerCrypto2FiatFee :: FeeRate 'Maker,
        makerDerivativeRebate :: RebateRate 'Maker,
        takerCrypto2CryptoFee :: FeeRate 'Taker,
        takerCrypto2StableFee :: FeeRate 'Taker,
        takerCrypto2FiatFee :: FeeRate 'Taker,
        takerDerivativeFee :: FeeRate 'Taker
      }
  deriving (Eq, Ord, Show)

instance FromRpc 'FeeSummary a Response where
  fromRpc _ res@(RawResponse raw) = do
    x0 <- parse 0 0 newFeeRate "makerCrypto2CryptoFee"
    x1 <- parse 0 1 newFeeRate "makerCrypto2StableFee"
    x2 <- parse 0 2 newFeeRate "makerCrypto2FiatFee"
    x3 <- parse 0 5 (pure . RebateRate) "makerDerivativeRebate"
    x4 <- parse 1 0 newFeeRate "takerCrypto2CryptoFee"
    x5 <- parse 1 1 newFeeRate "takerCrypto2StableFee"
    x6 <- parse 1 2 newFeeRate "takerCrypto2FiatFee"
    x7 <- parse 1 5 newFeeRate "takerDerivativeFee"
    pure $
      Response x0 x1 x2 x3 x4 x5 x6 x7
    where
      parse ix0 ix1 con field =
        (con =<<)
          . (toRational <$>)
          . maybeToRight
            ( fromRpcError FeeSummary res $
                field <> " is missing"
            )
          $ raw ^? nth 4 . nth ix0 . nth ix1 . _Number
