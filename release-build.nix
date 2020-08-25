{
  buildNum ? null
}:

let
  mkWindows = cluster: (import ./. { inherit cluster buildNum; target = "x86_64-windows"; HSMServer = "HSM"; }).windows-installer;
  mkLinux = cluster: (import ./. { inherit cluster buildNum;}).wrappedBundle;
  pkgs = (import ./. {}).pkgs;
in pkgs.runCommand "signed-release" {} ''
  mkdir $out
  cp -v ${mkLinux "mainnet"}/*bin $out/
  cp -v ${mkWindows "mainnet"}/*exe $out/
''
