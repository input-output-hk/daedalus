with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/6a8790429692280998801c96660dcc85e30fb759.tar.gz) {});

stdenv.mkDerivation {
  name = "daedalus";

  buildInputs = [electron nodejs-6_x nodePackages.bower nodePackages.node-gyp nodePackages.node-pre-gyp ];

  src = null;

}
