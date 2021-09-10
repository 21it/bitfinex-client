{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module BitfinexClient.Rpc.Generic
  ( Rpc (..),
    pub,
    prv,
  )
where

import BitfinexClient.Class.ToPathPieces
import BitfinexClient.Import
import qualified Crypto.Hash as Crypto (Digest)
import qualified Crypto.Hash.Algorithms as Crypto (SHA384)
import qualified Crypto.MAC.HMAC as Crypto (hmac, hmacGetDigest)
import qualified Data.Aeson as A
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Network.HTTP.Client as Web
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Web

data Rpc (method :: Method)
  = Rpc

pub ::
  forall m method req res.
  ( MonadIO m,
    ToBaseUrl method,
    ToPathPieces method req,
    ToRequestMethod method,
    FromRpc method req res
  ) =>
  Rpc method ->
  [SomeQueryParam] ->
  req ->
  ExceptT Error m res
pub Rpc qs req = catchWeb $ do
  manager <-
    Web.newManager Tls.tlsManagerSettings
  webReq0 <-
    Web.parseRequest
      . T.unpack
      . T.intercalate "/"
      $ coerce (toBaseUrl @method) : toPathPieces @method req
  let webReq1 =
        Web.setQueryString
          (unQueryParam <$> qs)
          $ webReq0 {Web.method = show $ toRequestMethod @method}
  webRes <-
    Web.httpLbs webReq1 manager
  pure $
    if Web.responseStatus webRes == Web.ok200
      then fromRpc @method req . RawResponse $ Web.responseBody webRes
      else Left $ ErrorWebPub webReq1 webRes

prv ::
  forall m method req res.
  ( MonadIO m,
    ToBaseUrl method,
    ToPathPieces method req,
    ToRequestMethod method,
    ToJSON req,
    FromRpc method req res
  ) =>
  Rpc method ->
  Env ->
  req ->
  ExceptT Error m res
prv Rpc env req = catchWeb $ do
  manager <-
    Web.newManager Tls.tlsManagerSettings
  let apiPath =
        T.intercalate "/" $ toPathPieces @method req
  nonce <- encodeUtf8 <$> (show <$> newNonce :: IO Text)
  let reqBody = A.encode req
  webReq0 <-
    Web.parseRequest
      . T.unpack
      $ coerce (toBaseUrl @method) <> "/" <> apiPath
  let webReq1 =
        webReq0
          { Web.method = show $ toRequestMethod @method,
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
                  B16.encode
                    . BS.pack
                    . BA.unpack
                    $ sign (envPrvKey env) apiPath nonce reqBody
                )
              ]
          }
  webRes <-
    Web.httpLbs webReq1 manager
  pure $
    if Web.responseStatus webRes == Web.ok200
      then fromRpc @method req . RawResponse $ Web.responseBody webRes
      else Left $ ErrorWebPrv reqBody webReq1 webRes

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
