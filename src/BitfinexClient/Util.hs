module BitfinexClient.Util
  ( someExchangeRateCurrencyPair,
    fromRpcError,
    newNonce,
    unOrderFlag,
    unOrderFlagSet,
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Import.External
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

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

newNonce :: MonadIO m => m Nonce
newNonce = liftIO $ Nonce . utcTimeToMicros <$> getCurrentTime

utcTimeToMicros :: UTCTime -> Integer
utcTimeToMicros x =
  diffTimeToPicoseconds
    ( fromRational
        . toRational
        $ diffUTCTime x epoch
    )
    `div` 1000000

epoch :: UTCTime
epoch = posixSecondsToUTCTime 0

unOrderFlag :: OrderFlag -> OrderFlagAcc
unOrderFlag = OrderFlagAcc . \case
  Hidden -> 64
  Close -> 512
  ReduceOnly -> 1024
  PostOnly -> 4096
  Oco -> 16384
  NoVarRates -> 524288

unOrderFlagSet :: Set OrderFlag -> OrderFlagAcc
unOrderFlagSet =
  foldr
    (\x acc -> acc + unOrderFlag x)
    $ OrderFlagAcc 0
