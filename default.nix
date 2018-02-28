<<<<<<< HEAD
with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/ffea68a09cedab941d19f02c9041689ebc81192e.tar.gz) {});
# NOTE: when bumping nixpkgs, also update .travis.yaml
=======
with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/fb235c98d839ae37a639695ad088d19ef8382608.tar.gz) {});
# NOTE: when bumping nixpkgs, also update nixpkgs-src.json and .travis.yml
>>>>>>> release/0.9.0

stdenv.mkDerivation {
  name = "daedalus";

<<<<<<< HEAD
  buildInputs = [electron nodejs-8_x nodePackages.bower nodePackages.node-gyp nodePackages.node-pre-gyp ];
=======
  buildInputs = [
    nix bash binutils coreutils curl
    git python27 curl electron nodejs-6_x
    nodePackages.node-gyp nodePackages.node-pre-gyp
    gnumake
  ];
>>>>>>> release/0.9.0

  src = null;

}
