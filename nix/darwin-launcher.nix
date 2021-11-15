{ runCommand
, go
, symlinkJoin
, writeTextFile
, runtimeShell
, writeShellScriptBin
}:

let
  darwin-launcher = runCommand "darwin-launcher" {
    buildInputs = [ go ];
    passthru = { inherit test; };
  } ''
    export HOME=$NIX_BUILD_TOP
    go env -w GO111MODULE=off
    mkdir -p $out/bin
    cp ${./darwin-launcher.go} darwin-launcher.go
    CGO_ENABLED=0 go build -a -o $out/bin/darwin-launcher
  '';

  # To test darwin-launcher.go, run
  #   nix-build -A darwin-launcher.test && ./result/bin/darwin-launcher
  # There will be no output and exit code 0 if it passes.
  test = symlinkJoin {
    name = "darwin-launcher-test";
    paths = [
      darwin-launcher
      (writeShellScriptBin "cardano-launcher" ''
        set -euo pipefail
        echo "Checking that helper was run:"
        test -f .helper-was-here
        rm .helper-was-here
        echo "Try starting a bundled program:"
        hello
        echo "PASS"
      '')
      (writeShellScriptBin "hello" ''echo Hello'')
      (writeTextFile {
        name = "helper";
        text = ''
          #!${runtimeShell}
          echo "Helper started"
          touch .helper-was-here
        '';
        executable = true;
        destination = "/Resources/helper";
      })
    ];
    postBuild = ''
      # Copy symlink targets into the output, because
      # os.Executable() is not fooled by symlinks.
      find $out -type l | while read symlink; do
        cp --remove-destination $(readlink $symlink) $symlink
      done
    '';
  };

in
  darwin-launcher
