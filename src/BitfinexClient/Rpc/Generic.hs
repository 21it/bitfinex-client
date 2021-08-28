{-# LANGUAGE GADTs #-}

module BitfinexClient.Rpc.Generic
  ( pub,
    prv,
  )
where

import BitfinexClient.Import
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Network.HTTP.Client as Web
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Web

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
    ToBaseUrl method,
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
      $ coerce (toBaseUrl rpc) : toPathPiece rpc
  let webReq1 =
        Web.setQueryString
          (unQueryParam <$> qs)
          $ webReq0 {Web.method = show $ toRequestMethod rpc}
  webRes <-
    Web.httpLbs webReq1 manager
  pure $
    if Web.responseStatus webRes == Web.ok200
      then fromRpc rpc req . RawResponse $ Web.responseBody webRes
      else Left $ ErrorWebResponse webReq1 webRes

prv ::
  ( MonadIO m,
    ToBaseUrl method,
    ToPathPieces rpc,
    ToRequestMethod method,
    ToJSON req,
    FromRpc method req res,
    rpc ~ Rpc method
  ) =>
  rpc ->
  req ->
  ExceptT Error m res
prv rpc req = catchWeb $ do
  manager <-
    Web.newManager Tls.tlsManagerSettings
  webReq0 <-
    Web.parseRequest
      . T.unpack
      . T.intercalate "/"
      $ coerce (toBaseUrl rpc) : toPathPiece rpc
  let webReq1 =
        webReq0
          { Web.method = show $ toRequestMethod rpc,
            Web.requestBody = Web.RequestBodyLBS $ A.encode req
          }
  webRes <-
    Web.httpLbs webReq1 manager
  pure $
    if Web.responseStatus webRes == Web.ok200
      then fromRpc rpc req . RawResponse $ Web.responseBody webRes
      else Left $ ErrorWebResponse webReq1 webRes
