{ stdenv, lib, libXScrnSaver, makeWrapper, fetchurl, unzip, atomEnv, libuuid, at-spi2-atk, at_spi2_core, libxshmfence,
  libdrm, libxkbcommon, mesa }:

let
  version = "13.1.0";
  name = "electron-${version}";

  throwSystem = throw "Unsupported system: ${stdenv.hostPlatform.system}";

  meta = with lib; {
    description = "Cross platform desktop application shell";
    homepage = https://github.com/electron/electron;
    license = licenses.mit;
    maintainers = with maintainers; [ travisbhartwell manveru ];
    platforms = [ "x86_64-darwin" "aarch64-darwin" "x86_64-linux" "i686-linux" ];
  };

  linux = {
    inherit name version meta;

    src = {
      i686-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-ia32.zip";
        sha256 = "e4d8cc19689b1f9c00894bb776083f8543abeba9203d3297268c1644f1300355";
      };
      x86_64-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-x64.zip";
        sha256 = "6681078971d2e99e9f8b3c598de65869d3596356af901bbbabcc4860c0496bb4";
      };
      armv7l-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-armv7l.zip";
        sha256 = "ac5a19e31fd83db5775a2af57f742c7b3e5fc4528958329ec3c81f82e7bd611a";
      };
      aarch64-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-arm64.zip";
        sha256 = "0ed0cc3afae5cc7e1e6c6204866a1cecdf97e0ab658789e8951842bd0d28e1bb";
      };
    }.${stdenv.hostPlatform.system} or throwSystem;

    buildInputs = [ unzip makeWrapper ];

    buildCommand = ''
      mkdir -p $out/lib/electron $out/bin
      unzip -d $out/lib/electron $src
      ln -s $out/lib/electron/electron $out/bin

      fixupPhase

      patchelf \
        --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
        --set-rpath "${atomEnv.libPath}:${lib.makeLibraryPath [ libuuid at-spi2-atk at_spi2_core ]}:$out/lib/electron" \
        $out/lib/electron/electron

      wrapProgram $out/lib/electron/electron \
        --prefix LD_PRELOAD : ${lib.makeLibraryPath [ libXScrnSaver ]}/libXss.so.1 \
        --prefix LD_PRELOAD : ${lib.makeLibraryPath [ libdrm ]}/libdrm.so.2 \
        --prefix LD_PRELOAD : ${lib.makeLibraryPath [ libxkbcommon ]}/libxkbcommon.so.0 \
        --prefix LD_PRELOAD : ${lib.makeLibraryPath [ mesa ]}/libgbm.so.1 \
        --prefix LD_PRELOAD : ${lib.makeLibraryPath [ libxshmfence ]}/libxshmfence.so.1
    '';
  };

  darwin = {
    inherit name version meta;

    src = {
      x86_64-darwin = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-darwin-x64.zip";
        sha256 = "d62a561e80fcbcb0f249e74c487313192451046f288478add65be997793831de";
      };
      aarch64-darwin = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-darwin-arm64.zip";
        sha256 = "6da46d2861011263af2953a3e0186735e54708c01c50967ff5f8ed71b73f7fea";
      };
    }.${stdenv.hostPlatform.system} or throwSystem;

    buildInputs = [ unzip ];

    buildCommand = ''
      mkdir -p $out/Applications
      unzip $src
      mv Electron.app $out/Applications
      mkdir -p $out/bin
      ln -s $out/Applications/Electron.app/Contents/MacOs/Electron $out/bin/electron
    '';
  };
in

  stdenv.mkDerivation (if stdenv.isDarwin then darwin else linux)
