module BitfinexClient.Util
  ( someExchangeRateCurrencyPair,
    fromRpcError,
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Import.External

someExchangeRateCurrencyPair :: SomeExchangeRate -> CurrencyPair
someExchangeRateCurrencyPair x =
  CurrencyPair
    { currencyPairBase = CurrencyCode $ someExchangeRateSrcCurrency x,
      currencyPairQuote = CurrencyCode $ someExchangeRateDstCurrency x
    }

fromRpcError :: Method -> RawResponse -> Text -> Error
fromRpcError method res err =
  ErrorFromRpc $
    show method
      <> " FromRpc failed because "
      <> err
      <> " in "
      <> show res
