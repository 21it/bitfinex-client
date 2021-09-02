module BitfinexClient.Data.FeeSummary
  ( Response (..),
  )
where

import BitfinexClient.Import
import Data.Aeson.Lens

data Response
  = Response
      { makerCrypto2CryptoFee :: Rational,
        makerCrypto2StableFee :: Rational,
        makerCrypto2FiatFee :: Rational,
        makerDerivativeRebate :: Rational,
        takerCrypto2CryptoFee :: Rational,
        takerCrypto2StableFee :: Rational,
        takerCrypto2FiatFee :: Rational,
        takerDerivativeFee :: Rational
      }
  deriving (Eq, Ord, Show)

instance FromRpc 'FeeSummary a Response where
  fromRpc Rpc _ res@(RawResponse raw) = do
    x0 <- parse 0 0 "makerCrypto2CryptoFee"
    x1 <- parse 0 1 "makerCrypto2StableFee"
    x2 <- parse 0 2 "makerCrypto2FiatFee"
    x3 <- parse 0 5 "makerDerivativeRebate"
    x4 <- parse 1 0 "takerCrypto2CryptoFee"
    x5 <- parse 1 1 "takerCrypto2StableFee"
    x6 <- parse 1 2 "takerCrypto2FiatFee"
    x7 <- parse 1 5 "takerDerivativeFee"
    pure $
      Response x0 x1 x2 x3 x4 x5 x6 x7
    where
      parse ix0 ix1 field =
        (toRational <$>)
          . maybeToRight
            ( fromRpcError FeeSummary res $
                field <> " is missing"
            )
          $ raw ^? nth 4 . nth ix0 . nth ix1 . _Number
