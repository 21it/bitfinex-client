{ mkDerivation, aeson, async, base, base16-bytestring, bytestring
, chronos, co-log, concur-core, concur-replica, containers
, cryptonite, envparse, esqueleto, extra, file-embed, hpack, hspec
, hspec-wai, http-client, http-client-tls, http-types, katip, lens
, lens-aeson, memory, microlens, monad-logger, persistent
, persistent-migration, persistent-postgresql, persistent-template
, replica, resource-pool, retry, safe-money, stdenv, stm
, template-haskell, text, time, unbounded-delays, universum
, unliftio, vector, wai, wai-middleware-static-embedded, warp
, websockets
}:
mkDerivation {
  pname = "bitfinex-client";
  version = "0.1.0.0";
  src = ./..;
  libraryHaskellDepends = [
    aeson async base base16-bytestring bytestring chronos co-log
    concur-core concur-replica containers cryptonite envparse esqueleto
    extra file-embed hspec hspec-wai http-client http-client-tls
    http-types katip lens lens-aeson memory microlens monad-logger
    persistent persistent-migration persistent-postgresql
    persistent-template replica resource-pool retry safe-money stm
    template-haskell text time unbounded-delays universum unliftio
    vector wai wai-middleware-static-embedded warp websockets
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson async base base16-bytestring bytestring chronos co-log
    concur-core concur-replica containers cryptonite envparse esqueleto
    extra file-embed hspec hspec-wai http-client http-client-tls
    http-types katip lens lens-aeson memory microlens monad-logger
    persistent persistent-migration persistent-postgresql
    persistent-template replica resource-pool retry safe-money stm
    template-haskell text time unbounded-delays universum unliftio
    vector wai wai-middleware-static-embedded warp websockets
  ];
  prePatch = "hpack";
  homepage = "https://github.com/tkachuk-labs/bitfinex-client#readme";
  license = stdenv.lib.licenses.bsd3;
}
