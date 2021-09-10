{-# LANGUAGE AllowAmbiguousTypes #-}

module BitfinexClient.Class.FromRpc
  ( FromRpc (..),
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Import.External

class FromRpc (method :: Method) req res where
  fromRpc :: req -> RawResponse -> Either Error res
