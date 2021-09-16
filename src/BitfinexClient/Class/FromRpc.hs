{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module BitfinexClient.Class.FromRpc
  ( FromRpc (..),
  )
where

import BitfinexClient.Data.Kind
import BitfinexClient.Data.Type
import BitfinexClient.Data.Web
import BitfinexClient.Import.External

class FromRpc (method :: Method) req res where
  fromRpc :: req -> RawResponse -> Either Error res
