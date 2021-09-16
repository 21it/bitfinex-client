{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Util
  ( fromRpcError,
    eradicateNull,
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Data.Web
import BitfinexClient.Import.External
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HS
import qualified Data.Vector as V

fromRpcError :: Method -> RawResponse -> Text -> Error
fromRpcError method res err =
  ErrorFromRpc $
    show method
      <> " FromRpc failed because "
      <> err
      <> " in "
      <> show res

eradicateNull :: A.Value -> A.Value
eradicateNull = \case
  A.Object xs -> A.Object $ HS.mapMaybe devastateNull xs
  A.Array xs -> A.Array $ V.mapMaybe devastateNull xs
  x -> x
  where
    devastateNull =
      \case
        A.Null -> Nothing
        x -> Just $ eradicateNull x
