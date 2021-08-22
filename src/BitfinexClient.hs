module BitfinexClient
  ( marketAveragePrice,
  )
where

import qualified BitfinexClient.Data.MarketAveragePrice as MarketAveragePrice
import BitfinexClient.Import
import qualified BitfinexClient.Rpc.Generic as GenericRpc

marketAveragePrice ::
  MonadIO m =>
  CurrencyPair ->
  Rational ->
  ExceptT Error m SomeExchangeRate
marketAveragePrice symbol amount =
  GenericRpc.pub
    (Rpc :: Rpc 'MarketAveragePrice)
    MarketAveragePrice.Request
      { MarketAveragePrice.symbol = symbol,
        MarketAveragePrice.amount = amount
      }
    [ SomeQueryParam "symbol" symbol,
      SomeQueryParam "amount" amount
    ]
