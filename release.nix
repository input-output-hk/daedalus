let
  localLib = import ./lib.nix;
in

{ supportedSystems ? ["x86_64-linux" "x86_64-darwin"]
, scrubJobs ? true
, nixpkgsArgs ? {
    config = { allowUnfree = false; inHydra = true; };
    inherit buildNum;
  }
, buildNum ? null
}:

with (import (localLib.fetchNixPkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
  packageSet = import ./.;
});

{
  inherit (import ./. {}) tests;

} // mapTestOn {
  daedalus = supportedSystems;

  mainnet.appImage = [ "x86_64-linux" ];
  staging.appImage = [ "x86_64-linux" ];
  testnet.appImage = [ "x86_64-linux" ];

  mainnet.linuxInstaller = [ "x86_64-linux" ];
  staging.linuxInstaller = [ "x86_64-linux" ];
  testnet.linuxInstaller = [ "x86_64-linux" ];
}
