{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Class.FromRpc
  ( FromRpc (..),
  )
where

import qualified BitfinexClient.Data.FeeSummary as FeeSummary
import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Data.Web
import BitfinexClient.Import.External
import BitfinexClient.Parser
import BitfinexClient.Util
import Data.Aeson.Lens

class FromRpc (method :: Method) res where
  fromRpc :: RawResponse -> Either Error res

instance FromRpc 'CancelOrderMulti (Map OrderId (Order 'Remote)) where
  fromRpc res@(RawResponse raw) = do
    xs <-
      maybeToRight
        ( fromRpcError
            CancelOrderMulti
            res
            "Order Array is missing"
        )
        $ raw ^? nth 4
    parseOrderMap xs

instance FromRpc 'RetrieveOrders (Map OrderId (Order 'Remote)) where
  fromRpc (RawResponse raw) = parseOrderMap raw

instance FromRpc 'OrdersHistory (Map OrderId (Order 'Remote)) where
  fromRpc (RawResponse raw) = parseOrderMap raw

instance FromRpc 'SubmitOrder (Order 'Remote) where
  fromRpc res@(RawResponse raw) = do
    order <-
      maybeToRight
        (fromRpcError SubmitOrder res "Order is missing")
        $ raw ^? nth 4 . nth 0
    parseOrder order

instance FromRpc 'MarketAveragePrice ExchangeRate where
  fromRpc res@(RawResponse raw) = do
    x <-
      maybeToRight
        (fromRpcError MarketAveragePrice res "ExchangeRate is missing")
        (toRational <$> raw ^? nth 0 . _Number)
    newExchangeRate x

instance FromRpc 'FeeSummary FeeSummary.Response where
  fromRpc res@(RawResponse raw) = do
    x0 <- parse 0 0 newFeeRate "makerCrypto2CryptoFee"
    x1 <- parse 0 1 newFeeRate "makerCrypto2StableFee"
    x2 <- parse 0 2 newFeeRate "makerCrypto2FiatFee"
    x3 <- parse 0 5 (pure . RebateRate) "makerDerivativeRebate"
    x4 <- parse 1 0 newFeeRate "takerCrypto2CryptoFee"
    x5 <- parse 1 1 newFeeRate "takerCrypto2StableFee"
    x6 <- parse 1 2 newFeeRate "takerCrypto2FiatFee"
    x7 <- parse 1 5 newFeeRate "takerDerivativeFee"
    pure $
      FeeSummary.Response x0 x1 x2 x3 x4 x5 x6 x7
    where
      parse ix0 ix1 con field =
        (con =<<)
          . (toRational <$>)
          . maybeToRight
            ( fromRpcError FeeSummary res $
                field <> " is missing"
            )
          $ raw ^? nth 4 . nth ix0 . nth ix1 . _Number
