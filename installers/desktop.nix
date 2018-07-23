{ runCommand, daedalus, desktopItem, network }:

let
  localLib = import ../lib.nix;
  icons = {
    mainnet = ./icons/mainnet/1024x1024.png;
    staging = ./icons/staging.iconset/icon_512x512.png;
    testnet = ./icons/testnet.iconset/icon_512x512.png;
  };
  icon = icons.${network};
  iconName = localLib.daedalusProgName network;
in
  daedalus.overrideAttrs (oldAttrs: {
    passthru = { inherit network; };
    buildCommand = ''
      ${oldAttrs.buildCommand}
      mkdir -p $out/share/icons/hicolor/1024x1024/apps
      ln -s ${desktopItem}/share/applications $out/share
      ln -s ${icon} $out/share/icons/hicolor/1024x1024/apps/${iconName}.png
    '';
  })
