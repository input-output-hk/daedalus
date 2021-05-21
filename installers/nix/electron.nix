{ stdenv, libXScrnSaver, makeWrapper, fetchurl, unzip, atomEnv, libuuid, at-spi2-atk, at_spi2_core, libxshmfence,
  libdrm, libxkbcommon, mesa }:

let
  version = "12.0.9";
  name = "electron-${version}";

  throwSystem = throw "Unsupported system: ${stdenv.hostPlatform.system}";

  meta = with stdenv.lib; {
    description = "Cross platform desktop application shell";
    homepage = https://github.com/electron/electron;
    license = licenses.mit;
    maintainers = with maintainers; [ travisbhartwell manveru ];
    platforms = [ "x86_64-darwin" "arm64-darwin" "x86_64-linux" "i686-linux" "armv7l-linux" "aarch64-linux" ];
  };

  linux = {
    inherit name version meta;

    src = {
      i686-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-ia32.zip";
        sha256 = "37405b3b27779ad417c3ae432d7f0d969c126c958a0ad8f2585c34fc8ee6c6e6";
      };
      x86_64-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-x64.zip";
        sha256 = "3ab0a873f720d3bf56cce6ca1bf9d8b956843920798f659ca0829e4cc3126f6d";
      };
      armv7l-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-armv7l.zip";
        sha256 = "2d41ef3ed6a215efe2c7d03d8055fcfda0079f09e9580e5bf70e8ac4a22b0898";
      };
      aarch64-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-arm64.zip";
        sha256 = "22a85817ea2edbba2e17b35f6e3a8104b2165e070ea21a1f2fa3b40e8d7aecc9";
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
        --set-rpath "${atomEnv.libPath}:${stdenv.lib.makeLibraryPath [ libuuid at-spi2-atk at_spi2_core ]}:$out/lib/electron" \
        $out/lib/electron/electron

      wrapProgram $out/lib/electron/electron \
        --prefix LD_PRELOAD : ${stdenv.lib.makeLibraryPath [ libXScrnSaver ]}/libXss.so.1 \
        --prefix LD_PRELOAD : ${stdenv.lib.makeLibraryPath [ libdrm ]}/libdrm.so.2 \
        --prefix LD_PRELOAD : ${stdenv.lib.makeLibraryPath [ libxkbcommon ]}/libxkbcommon.so.0 \
        --prefix LD_PRELOAD : ${stdenv.lib.makeLibraryPath [ mesa ]}/libgbm.so.1 \
        --prefix LD_PRELOAD : ${stdenv.lib.makeLibraryPath [ libxshmfence ]}/libxshmfence.so.1
    '';
  };

  darwin = {
    inherit name version meta;

    src = {
      x86_64-darwin = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-darwin-x64.zip";
        sha256 = "b3f1e378f58e7c36b54451c5a3485adc370277827974e1eb0790b6965737c872";
      };
      arm64-darwin = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-darwin-arm64.zip";
        sha256 = "22a85817ea2edbba2e17b35f6e3a8104b2165e070ea21a1f2fa3b40e8d7aecc9";
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
