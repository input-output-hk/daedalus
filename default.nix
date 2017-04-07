with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/09c7601c204016c8ecdefd474f721fb841e834df.tar.gz) {});
# NOTE: when bumping nixpkgs, also update .travis.yaml

stdenv.mkDerivation {
  name = "daedalus";

  buildInputs = [electron nodejs-6_x nodePackages.bower nodePackages.node-gyp nodePackages.node-pre-gyp ];

  src = null;

}
