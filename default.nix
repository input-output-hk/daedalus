with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/d4787680bcc9c5163eec15756e871044b2220b4e.tar.gz) {});

stdenv.mkDerivation {
  name = "daedalus";

  buildInputs = [electron nodejs nodePackages.bower nodePackages.node-gyp nodePackages.node-pre-gyp ];

  src = ./app;

}
