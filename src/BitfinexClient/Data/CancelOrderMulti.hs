{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Data.CancelOrderMulti
  ( Request (..),
  )
where

import BitfinexClient.Import
import qualified Data.Aeson as A
import Data.Aeson.Lens

data Request
  = ByOrderId (Set OrderId)
  | ByOrderClientId (Set (OrderClientId, UTCTime))
  | ByOrderGroupId (Set OrderGroupId)
  | Everything
  deriving (Eq, Ord, Show)

instance ToJSON Request where
  toJSON = eradicateNull . A.object . \case
    ByOrderId xs -> ["id" A..= toJSON xs]
    --
    -- TODO : verify
    --
    ByOrderClientId xs -> ["cid" A..= toJSON (second utctDay <$> toList xs)]
    ByOrderGroupId xs -> ["gid" A..= toJSON xs]
    Everything -> ["all" A..= toJSON (1 :: Int)]

instance FromRpc 'CancelOrderMulti Request (Map OrderId (Order 'Remote)) where
  fromRpc _ res@(RawResponse raw) = do
    xs <-
      maybeToRight
        ( fromRpcError
            CancelOrderMulti
            res
            "Order Array is missing"
        )
        $ raw ^? nth 4
    parseOrderMap xs
