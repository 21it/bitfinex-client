{-# LANGUAGE GADTs #-}

module BitfinexClient.Rpc.Generic
  ( pub,
    prv,
  )
where

import BitfinexClient.Import
import qualified Crypto.Hash as Crypto (Digest)
import qualified Crypto.Hash.Algorithms as Crypto (SHA384)
import qualified Crypto.MAC.HMAC as Crypto (hmac, hmacGetDigest)
import qualified Data.Aeson as A
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Network.HTTP.Client as Web
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Web

catchWeb ::
  (MonadIO m) =>
  IO (Either Error a) ->
  ExceptT Error m a
catchWeb this =
  --
  -- TODO : hide sensitive data like headers
  --
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
  Env ->
  req ->
  ExceptT Error m res
prv rpc env req = catchWeb $ do
  manager <-
    Web.newManager Tls.tlsManagerSettings
  let apiPath =
        T.intercalate "/" $ toPathPiece rpc
  nonce <- encodeUtf8 <$> (show <$> newNonce :: IO Text)
  let reqBody = A.encode req
  webReq0 <-
    Web.parseRequest
      . T.unpack
      $ coerce (toBaseUrl rpc) <> "/" <> apiPath
  let webReq1 =
        webReq0
          { Web.method = show $ toRequestMethod rpc,
            Web.requestBody = Web.RequestBodyLBS reqBody,
            Web.requestHeaders =
              [ ( "Content-Type",
                  "application/json"
                ),
                ( "bfx-nonce",
                  nonce
                ),
                ( "bfx-apikey",
                  coerce $ envApiKey env
                ),
                ( "bfx-signature",
                  BS.pack
                    . BA.unpack
                    $ sign (envPrvKey env) apiPath nonce reqBody
                )
              ]
          }
  webRes <-
    Web.httpLbs webReq1 manager
  pure $
    if Web.responseStatus webRes == Web.ok200
      then fromRpc rpc req . RawResponse $ Web.responseBody webRes
      else Left $ ErrorWebResponse webReq1 webRes

sign ::
  PrvKey ->
  Text ->
  BS.ByteString ->
  ByteString ->
  Crypto.Digest Crypto.SHA384
sign prvKey apiPath nonce reqBody =
  Crypto.hmacGetDigest
    . Crypto.hmac (coerce prvKey :: BS.ByteString)
    $ "/api/"
      <> encodeUtf8 apiPath
      <> nonce
      <> BL.toStrict reqBody
