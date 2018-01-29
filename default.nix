with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/ffea68a09cedab941d19f02c9041689ebc81192e.tar.gz) {});
# NOTE: when bumping nixpkgs, also update .travis.yaml

stdenv.mkDerivation {
  name = "daedalus";

  buildInputs = [electron nodejs-8_x nodePackages.bower nodePackages.node-gyp nodePackages.node-pre-gyp ];

  src = null;

}
