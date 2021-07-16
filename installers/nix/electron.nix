{ stdenv, libXScrnSaver, makeWrapper, fetchurl, unzip, atomEnv, libuuid, at-spi2-atk, at_spi2_core, libxshmfence,
  libdrm, libxkbcommon, mesa }:

let
  version = "13.1.7";
  name = "electron-${version}";

  throwSystem = throw "Unsupported system: ${stdenv.hostPlatform.system}";

  meta = with stdenv.lib; {
    description = "Cross platform desktop application shell";
    homepage = https://github.com/electron/electron;
    license = licenses.mit;
    maintainers = with maintainers; [ travisbhartwell manveru ];
    platforms = [ "x86_64-darwin" "x86_64-linux" "i686-linux" "armv7l-linux" "aarch64-linux" ];
  };

  linux = {
    inherit name version meta;

    src = {
      i686-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-ia32.zip";
        sha256 = "2a1c84ca8fd2a5b10b918bda11c5e546f4b77f85484a32af24ed44d6f877587d";
      };
      x86_64-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-x64.zip";
        sha256 = "0bb38a5e45609a8c46dd6173447a45477651f3c2ea58f724807d79c8e4a8876e";
      };
      armv7l-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-armv7l.zip";
        sha256 = "3d4ed4cbd2ea9dd01d5ad09ed5b408762c69b5827be6fdae2e19681f2a159509";
      };
      aarch64-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-arm64.zip";
        sha256 = "68e174bee2a686926ec2da193831aefc16ff8ec43b46e423044918e6d25d5925";
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
        sha256 = "be8d05a7f853b9e7020c095c3d8075269832ccf821ca9785135884e6bc893df8";
      };
      aarch64-darwin = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-darwin-arm64.zip";
        sha256 = "95489cc66c5638d95cde80189a5ae3477ce09c6cfa4c421b1e8bceea94f4dfba";
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
