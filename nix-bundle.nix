{ pkgs, daedalus, nix-bundle }:

let
  #bundle = import "${nix-bundle}/share/nix-bundle" { nixpkgs = pkgs; };
  bundle = import ../nix-bundle { nixpkgs = pkgs; };
  foo = bundle.nix-bootstrap {
    target = "${daedalus}";
    run = "/bin/daedalus";
  };
in foo
