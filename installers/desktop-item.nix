{ makeDesktopItem, network }:

let
  localLib = import ../lib.nix;
  name = localLib.daedalusProgName network;
in makeDesktopItem {
    inherit name;
    exec = name;
    icon = name;
    desktopName = localLib.daedalusAppName network;
    genericName = "Crypto-Currency Wallet";
    categories = "Application;Network;";
    startupNotify = "true";
  } // {
    path = "share/applications/${name}.desktop";
  }
