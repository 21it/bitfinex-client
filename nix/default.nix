let nixpkgs = import ./nixpkgs.nix;
in
{
  pkgs ? import nixpkgs {
    overlays = import ./overlay.nix {
      inherit hexOrganization hexApiKey robotSshKey;
    };
  },
  hexOrganization ? null, # organization account name on hex.pm
  hexApiKey ? null,       # plain text account API key on hex.pm
  robotSshKey ? null      # base64-encoded private id_rsa (for private git)
}:
with pkgs;

let callPackage = lib.callPackageWith haskellPackages;
    pkg = callPackage ./pkg.nix {inherit stdenv;};
    systemDeps = [ protobuf makeWrapper cacert ];
    testDeps = [ postgresql ];
in
  haskell.lib.overrideCabal pkg (drv: {
    setupHaskellDepends =
      if drv ? "setupHaskellDepends"
      then drv.setupHaskellDepends ++ systemDeps
      else systemDeps;
    testSystemDepends =
      if drv ? "testSystemDepends"
      then drv.testSystemDepends ++ testDeps
      else testDeps;
    isExecutable = false;
    enableSharedExecutables = false;
    enableLibraryProfiling = false;
    isLibrary = true;
    doHaddock = false;
    prePatch = "hpack --force";
  })
