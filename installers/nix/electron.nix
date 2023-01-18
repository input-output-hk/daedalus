{ stdenv, lib, makeWrapper, fetchurl, unzip, atomEnv, libuuid, at-spi2-atk, at_spi2_core, libxshmfence,
  libxkbcommon, runCommand, binutils-unwrapped }:

let
  version = (builtins.fromJSON (builtins.readFile ../../package.json)).dependencies.electron;
  name = "electron-${version}";

  throwSystem = throw "Unsupported system: ${stdenv.hostPlatform.system}";

  meta = with lib; {
    description = "Cross platform desktop application shell";
    homepage = https://github.com/electron/electron;
    license = licenses.mit;
    maintainers = with maintainers; [ travisbhartwell manveru ];
    platforms = [ "x86_64-darwin" "aarch64-darwin" "x86_64-linux" "i686-linux" "armv7l-linux" "aarch64-linux" ];
  };

  linux = {
    inherit name version meta;

    src = {
      i686-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-ia32.zip";
        sha256 = "db9261c05ed57af2fcd4a84b89d299c76948b9d57ce0dba38e3240eb43935257";
      };
      x86_64-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-x64.zip";
        sha256 = "7607422a4ba80cda4bd7fefb2fbe2f4e0b9a73db92e1e82dc01012a85b5d0d2b";
      };
      armv7l-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-armv7l.zip";
        sha256 = "a293a9684e16a427a9f68d101814575a4b1dd232dc3fca47552f906019a6cadc";
      };
      aarch64-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-arm64.zip";
        sha256 = "1599d259832c806b98751a68fb93112711963d259024f0e36f12f064995b3251";
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
        --set-rpath "${atomEnv.libPath}:${lib.makeLibraryPath [ libuuid at-spi2-atk at_spi2_core libxshmfence libxkbcommon ]}:$out/lib/electron" \
        $out/lib/electron/electron
    '';
  };

  darwin = {
    inherit name version meta;

    src = {
      x86_64-darwin = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-darwin-x64.zip";
        sha256 = "6bf09794d6f020bbaaf806a7758da125137b3c96646f4503eb81b9541e50e02f";
      };
      aarch64-darwin = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-darwin-arm64.zip";
        sha256 = "374ddf0581794b31eee900828172f9218193c032c0e46bffcfac6aec95c22f1a";
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
