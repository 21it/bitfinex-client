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
  fromRpc Rpc req res@(RawResponse raw) = do
    obj <- fstError $ A.eitherDecode raw
    case obj of
      A.Array xs ->
        Set.fromList <$> mapM parser (toList xs)
      _ ->
        --
        -- TODO : use fromRpcError!!!
        --
        Left . ErrorFromRpc $
          "Json value is not an array " <> show res
    where
      fstError =
        first $ ErrorFromRpc . T.pack
      parser x = do
        id0 <-
          maybeToRight
            (fromRpcError RetrieveOrders res "OrderId is missing")
            $ x ^? nth 0 . _Integer
        amt <-
          maybeToRight
            (fromRpcError RetrieveOrders res "OrderAmount is missing")
            $ x ^? nth 7 . _Number
        ss0 <-
          maybeToRight
            (fromRpcError RetrieveOrders res "OrderStatus is missing")
            $ x ^? nth 13 . _String
        ss1 <-
          newOrderStatus ss0
        price <-
          maybeToRight
            (fromRpcError RetrieveOrders res "OrderAmount is missing")
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
