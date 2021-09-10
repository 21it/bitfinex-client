{ mkDerivation, aeson, base, base16-bytestring, bytestring
, containers, cryptonite, envparse, hpack, hspec, hspec-wai
, http-client, http-client-tls, http-types, lens-aeson, memory
, stdenv, text, time, transformers, universum, unliftio
}:
mkDerivation {
  pname = "bitfinex-client";
  version = "0.1.0.0";
  src = ./..;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring containers cryptonite
    envparse hspec hspec-wai http-client http-client-tls http-types
    lens-aeson memory text time transformers universum unliftio
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base base16-bytestring bytestring containers cryptonite
    envparse hspec hspec-wai http-client http-client-tls http-types
    lens-aeson memory text time transformers universum unliftio
  ];
  prePatch = "hpack";
  homepage = "https://github.com/tkachuk-labs/bitfinex-client#readme";
  license = stdenv.lib.licenses.bsd3;
}
