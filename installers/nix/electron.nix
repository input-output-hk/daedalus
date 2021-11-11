{ stdenv, lib, libXScrnSaver, makeWrapper, fetchurl, unzip, atomEnv, libuuid, at-spi2-atk, at_spi2_core, libxshmfence,
  libdrm, libxkbcommon, mesa }:

let
  version = "13.1.1";
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
        sha256 = "fed00edaaba0c4a615fe835baf7d0d0ff893dff902800006bf63cc994c24d3dd";
      };
      x86_64-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-x64.zip";
        sha256 = "eb6ae81d71a4d390ec5140d907b191a84c37621176eec9369bb6fc3bf8570e3b";
      };
      armv7l-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-armv7l.zip";
        sha256 = "7e745a38c6761fa9826b3b9b8d0bd060126a3949da6f3f09f11b842e5e22cee4";
      };
      aarch64-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-arm64.zip";
        sha256 = "445c88e9c9b33abbdb263103736fb5203938b0643bc5377fbdf12b444d26f211";
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
        sha256 = "1594ba9aa2e2aa059a03e6b70e16b8116de1998b38f8360801e113fa8d72938c";
      };
      aarch64-darwin = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-darwin-arm64.zip";
        sha256 = "7045538917c36214127b7f11a3223396c7199ac19e989e5648a0963773962e6c";
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
