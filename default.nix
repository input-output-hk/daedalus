with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/48ecdcf5980a6504cd3b884b121e29efb2fb83dc.tar.gz) {});
# NOTE: when bumping nixpkgs, also update .travis.yaml

stdenv.mkDerivation {
  name = "daedalus";

  buildInputs = [electron nodejs-6_x nodePackages.bower nodePackages.node-gyp nodePackages.node-pre-gyp ];

  src = null;

}
