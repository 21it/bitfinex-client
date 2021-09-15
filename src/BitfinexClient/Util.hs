module BitfinexClient.Util
  ( fromRpcError,
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Data.Web
import BitfinexClient.Import.External

fromRpcError :: Method -> RawResponse -> Text -> Error
fromRpcError method res err =
  ErrorFromRpc $
    show method
      <> " FromRpc failed because "
      <> err
      <> " in "
      <> show res
