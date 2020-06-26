{
  buildNum ? null
}:

let
  mkWindows = cluster: (import ./. { inherit cluster buildNum; target = "x86_64-windows"; HSMServer = "HSM"; }).windows-installer;
  mkLinux = cluster: (import ./release.nix { inherit buildNum;}).${cluster}.installer.x86_64-linux;
  pkgs = (import ./. {}).pkgs;
in pkgs.runCommand "signed-release" {} ''
  mkdir $out
  cp -v ${mkWindows "itn_rewards_v1"}/*exe $out/
''
