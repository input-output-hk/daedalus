{ lib, fetchzip, system }:
with lib;
let
  version = "1.8.7";
  fetchElectron = { platform, sha256}: fetchzip {
    name = "electron-${version}";
    url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-${platform}.zip";
    inherit sha256;
    stripRoot = false;
  };

  systems = {
    x86_64-linux = fetchElectron {
      platform = "linux-x64";
      sha256 = "1ki0j37x0qj93nf0591pwvp1xvw0dh17h506nyrl9w2qdp9ssyfr";
    };
    x86_64-darwin = fetchElectron {
      platform = "darwin-x64";
      sha256 = "1xx6vssl95894kk4qwjp158ax5by72b0gsj21kw9djrndlldpzi5";
    };
  };

in
  systems.${system}
