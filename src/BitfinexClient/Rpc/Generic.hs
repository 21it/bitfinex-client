{-# LANGUAGE GADTs #-}

module BitfinexClient.Rpc.Generic
  ( pub,
  )
where

import BitfinexClient.Import
import qualified Data.Text as T
import qualified Network.HTTP.Client as Web
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Web

baseUrl :: Text
baseUrl = "https://api.bitfinex.com/v2"

catchWeb ::
  (MonadIO m) =>
  IO (Either Error a) ->
  ExceptT Error m a
catchWeb this =
  ExceptT . liftIO $
    this
      `catch` (\(x :: HttpException) -> pure . Left $ ErrorWebException x)

pub ::
  ( MonadIO m,
    ToPathPieces rpc,
    ToRequestMethod method,
    FromRpc method req res,
    rpc ~ Rpc method
  ) =>
  rpc ->
  req ->
  [SomeQueryParam] ->
  ExceptT Error m res
pub rpc req qs = catchWeb $ do
  manager <-
    Web.newManager Tls.tlsManagerSettings
  webReq0 <-
    Web.parseRequest
      . T.unpack
      . T.intercalate "/"
      $ baseUrl : toPathPiece rpc
  let webReq1 =
        Web.setQueryString
          (unQueryParam <$> qs)
          $ webReq0 {Web.method = show $ toRequestMethod rpc}
  webRes <-
    Web.httpLbs webReq1 manager
  pure $
    if Web.responseStatus webRes == Web.ok200
      then fromRpc rpc req $ Web.responseBody webRes
      else Left $ ErrorWebResponse webReq1 webRes
