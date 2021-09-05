module BitfinexClient.Data.GetOrders
  ( Request (..),
  )
where

import BitfinexClient.Data.Order
import BitfinexClient.Import
import qualified Data.Aeson as A
import Data.Aeson.Lens
import qualified Data.Set as Set
import qualified Data.Text as T

data Request
  = Request
      { currencyPair :: CurrencyPair,
        orderIds :: Set OrderId
      }
  deriving (Eq, Ord, Show)

instance ToJSON Request where
  toJSON = toJSON . orderIds

instance FromRpc 'RetrieveOrders Request (Set Order) where
  fromRpc Rpc = parseOrderSet

instance FromRpc 'OrdersHistory Request (Set Order) where
  fromRpc Rpc = parseOrderSet

parseOrderSet :: Request -> RawResponse -> Either Error (Set Order)
parseOrderSet req res@(RawResponse raw) = do
  json <-
    first (failure . T.pack) $ A.eitherDecode raw
  case json of
    A.Array xs -> Set.fromList <$> mapM parser (toList xs)
    _ -> Left $ failure "Json value is not an array"
  where
    failure =
      fromRpcError RetrieveOrders res
    parser x = do
      id0 <-
        maybeToRight (failure "OrderId is missing") $
          x ^? nth 0 . _Integer
      amt <-
        maybeToRight (failure "OrderAmount is missing") $
          x ^? nth 7 . _Number
      ss0 <-
        maybeToRight (failure "OrderStatus is missing") $
          x ^? nth 13 . _String
      ss1 <-
        first failure $
          newOrderStatus ss0
      price <-
        maybeToRight
          (failure "ExchangeRate is missing")
          $ x ^? nth 16 . _Number
      rate <-
        newExchangeRate' (currencyPair req) (toRational price)
      pure
        Order
          { orderId = OrderId id0,
            orderRate = rate,
            orderAmount = toRational amt,
            orderStatus = ss1
          }
