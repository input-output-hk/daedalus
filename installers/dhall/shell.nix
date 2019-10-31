let
  root = import ../.. {};
  tester = root.pkgs.writeScriptBin "test-dhall" ''
    #!/bin/sh
    echo './launcher.dhall ./mainnet.dhall (./win64.dhall ./mainnet.dhall)' | dhall --explain
  '';
in root.pkgs.stdenv.mkDerivation {
  name = "dhall-shell";
  buildInputs = [ root.hsDaedalusPkgs.dhall.components.exes.dhall tester ];
}
