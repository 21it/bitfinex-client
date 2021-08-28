module BitfinexClient.Data.SubmitOrder
  ( Request (..),
  )
where

import BitfinexClient.Import
import qualified Data.Aeson as A
import Data.Aeson.Lens

data Request
  = Request
      { rate :: SomeExchangeRate,
        amount :: Rational
      }
  deriving (Eq, Ord, Show)

instance ToJSON Request where
  toJSON x =
    A.object
      [ "symbol"
          A..= ( toBodyParam $
                   someExchangeRateCurrencyPair rate0
               ),
        "price"
          A..= ( toBodyParam $
                   someExchangeRateRate rate0
               )
      ]
    where
      rate0 = rate x

instance FromRpc 'SubmitOrder Request Order where
  fromRpc Rpc req res@(RawResponse raw) = do
    id0 <-
      maybeToRight
        (fromRpcError SubmitOrder res "OrderId is missing")
        $ raw ^? nth 4 . nth 0 . nth 0 . _Integer
    pure
      Order
        { orderId = OrderId id0,
          orderRate = rate req,
          orderAmount = amount req
        }
