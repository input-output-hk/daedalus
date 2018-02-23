with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/fb235c98d839ae37a639695ad088d19ef8382608.tar.gz) {});
# NOTE: when bumping nixpkgs, also update nixpkgs-src.json and .travis.yml

stdenv.mkDerivation {
  name = "daedalus";

  buildInputs = [
    nix bash binutils coreutils curl
    git python27 curl electron nodejs-6_x
    nodePackages.node-gyp nodePackages.node-pre-gyp
    gnumake
  ];

  src = null;

}
