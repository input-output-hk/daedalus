with import (import ./fetchNixpkgs.nix (builtins.fromJSON (builtins.readFile ./nixpkgs-src.json))) {};

stdenv.mkDerivation {
  name = "daedalus";

  buildInputs = [
    nix bash binutils coreutils curl gnutar
    git python27 curl electron nodejs-6_x
    nodePackages.node-gyp nodePackages.node-pre-gyp
    gnumake
  ];

  src = null;

}
