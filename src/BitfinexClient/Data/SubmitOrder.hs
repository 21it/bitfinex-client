module BitfinexClient.Data.SubmitOrder
  ( Request (..),
  )
where

import BitfinexClient.Data.Order
import BitfinexClient.Import
import qualified Data.Aeson as A
import Data.Aeson.Lens

data Request
  = Request
      { rate :: ExchangeRate,
        amount :: Rational,
        flags :: Set OrderFlag
      }
  deriving (Eq, Ord, Show)

instance ToJSON Request where
  toJSON x =
    A.object
      [ "type"
          A..= ("EXCHANGE LIMIT" :: Text),
        "symbol"
          A..= toBodyParam (exchangeRatePair rate0),
        "price"
          A..= toBodyParam (exchangeRatePrice rate0),
        "amount"
          A..= toBodyParam (amount x),
        "flags"
          A..= unOrderFlagSet (flags x)
      ]
    where
      rate0 = rate x

instance FromRpc 'SubmitOrder Request Order where
  fromRpc Rpc req res@(RawResponse raw) = do
    id0 <-
      maybeToRight
        (fromRpcError SubmitOrder res "OrderId is missing")
        $ raw ^? nth 4 . nth 0 . nth 0 . _Integer
    ss0 <-
      maybeToRight
        (fromRpcError SubmitOrder res "OrderStatus is missing")
        $ raw ^? nth 4 . nth 0 . nth 13 . _String
    ss1 <-
      newOrderStatus ss0
    pure
      Order
        { orderId = OrderId id0,
          orderRate = rate req,
          orderAmount = amount req,
          orderStatus = ss1
        }
