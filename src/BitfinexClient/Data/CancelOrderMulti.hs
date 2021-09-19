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
  | ByOrderClientId (Set OrderClientId)
  | ByOrderGroupId (Set OrderGroupId)
  deriving (Eq, Ord, Show)

instance ToJSON Request where
  toJSON = eradicateNull . A.object . \case
    ByOrderId xs -> ["id" A..= toJSON xs]
    --
    -- TODO : fixme
    --
    ByOrderClientId xs -> ["cid" A..= toJSON xs]
    ByOrderGroupId xs -> ["gid" A..= toJSON xs]

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
