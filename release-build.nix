let
  mkWindows = cluster: (import ./. { inherit cluster; target = "x86_64-windows"; HSMServer = "HSM"; }).windows-installer;
  mkLinux = cluster: (import ./release.nix {}).${cluster}.installer.x86_64-linux;
  pkgs = (import ./. {}).pkgs;
in pkgs.runCommand "signed-release" {} ''
  mkdir $out
  cp -v ${mkWindows "mainnet"}/*exe $out/
  cp -v ${mkWindows "staging"}/*exe $out/
  cp -v ${mkLinux "mainnet"}/*bin $out/
  cp -v ${mkLinux "staging"}/*bin $out/
''
