{ stdenv, lib, makeWrapper, fetchurl, unzip, atomEnv, libuuid, at-spi2-atk, at_spi2_core, libxshmfence,
  libxkbcommon, runCommand, binutils-unwrapped }:

let
  version = (builtins.fromJSON (builtins.readFile ../../package.json)).dependencies.electron;
  name = "electron-${version}";

  # XXX: There should be a better way to get this info, but I haven’t found one:
  bundledNodeVersion = builtins.readFile (runCommand "electron-node-version" {} ''
    ${unzip}/bin/unzip ${allPlatforms.x86_64-linux} electron
    ${binutils-unwrapped}/bin/strings electron | grep -Po 'node-v\K.*?(?=-headers\.tar\.gz)' | tr -d '\n' >$out
  '');

  throwSystem = throw "Unsupported system: ${stdenv.hostPlatform.system}";

  meta = with lib; {
    description = "Cross platform desktop application shell";
    homepage = https://github.com/electron/electron;
    license = licenses.mit;
    maintainers = with maintainers; [ travisbhartwell manveru ];
    platforms = [ "x86_64-darwin" "aarch64-darwin" "x86_64-linux" "i686-linux" "armv7l-linux" "aarch64-linux" ];
  };

  allPlatforms = {
    i686-linux = fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-ia32.zip";
      sha256 = "0hlggn4ffs0fjgygc5akq23qy16dca29py594xck18qqwri2yp9f";
    };
    x86_64-linux = fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-x64.zip";
      sha256 = "1x5xb78lw3a9y19wgbb89vwghda36z45d0rw648a2ymq5s5lccy9";
    };
    armv7l-linux = fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-armv7l.zip";
      sha256 = "1v0z3mfiwxk6pz9hqrpv25rd7qx82y0k8y3q34zk5wvgmy207j2k";
    };
    aarch64-linux = fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-arm64.zip";
      sha256 = "0nayflxpmd9wjnqrj74k8lid92445bywq4v1qy7bdbj4rh8jlb0c";
    };
    x86_64-darwin = fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-darwin-x64.zip";
      sha256 = "15gk17wfrw2fwvdiidcbh8cxby4ain5y9i1ciygf8qjpk01wasvm";
    };
    aarch64-darwin = fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-darwin-arm64.zip";
      sha256 = "0vw77az374cgmncki46pff18w0izp5i00q8z667i6rbh4nj1sqcs";
    };
  };

  linux = {
    inherit name version meta bundledNodeVersion;

    src = allPlatforms.${stdenv.hostPlatform.system} or throwSystem;

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
    inherit name version meta bundledNodeVersion;

    src = allPlatforms.${stdenv.hostPlatform.system} or throwSystem;

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
