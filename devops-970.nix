{ cluster ? "staging", signingKeys ? null, fudgeConfig ? null, dummyInstaller ? true }:

let
  old = import ./. { inherit cluster; };
  inherit (old) pkgs;
  inherit (pkgs) lib;
  newpkgssrc = pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs";
    rev = "be445a9074f";
    sha256 = "15dc7gdspimavcwyw9nif4s59v79gk18rwsafylffs9m1ld2dxwa";
  };
  newpkgs = import newpkgssrc {
    #crossSystem = (import "${newpkgssrc}/lib").systems.examples.mingwW64;
    #localSystem.system = "x86_64-linux";
  };
  packages = self: {
    inherit old;
    dlls = pkgs.fetchurl {
      url = "https://s3.eu-central-1.amazonaws.com/daedalus-ci-binaries/DLLs.zip";
      sha256 = "0p6nrf8sg2wgcaf3b1qkbb98bz2dimb7lnshsa93xnmia9m2vsxa";
    };
    nsis = newpkgs.callPackage ./nsis.nix {};
    cardanoJSON = builtins.fromJSON (builtins.readFile ./cardano-sl-src.json);
    cardanoSrc = pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "cardano-sl";
      rev = self.cardanoJSON.rev;
      sha256 = self.cardanoJSON.sha256;
    };
    crossCompiledCardano = (import (self.cardanoSrc + "/release.nix") { cardano = { outPath = self.cardanoSrc; rev = self.cardanoJSON.rev; }; }).daedalus-mingw32-pkg;
    unsignedUnpackedCardano = pkgs.runCommand "daedalus-bridge" { buildInputs = [ pkgs.unzip ]; } ''
      mkdir $out
      cd $out
      unzip ${self.crossCompiledCardano}/CardanoSL.zip
    '';
    signFile = file: let
      signingScript = pkgs.writeScript "signing-script" ''
        #!${pkgs.stdenv.shell}

        exec 3>&1
        exec 1>&2

        export PATH=${pkgs.mono}/bin:$PATH
        PASS=hunter2

        DIR=$(realpath $(mktemp -d))
        cd $DIR
        cp ${file} .
        FILE=$(basename ${file})
        chmod +w $FILE

        # if stdout is a tty, then mono 5.8 will barf over the terminfo files being too new
        # mono 5.16 supports them, but isnt in our current nixpkgs
        # for more info, refer to `mcs/class/corlib/System/TermInfoReader.cs` and `ReadHeader`
        echo $PASS | signcode -spc ${toString signingKeys.spc} -v ${toString signingKeys.pvk} -a sha1 -$ commercial -n "TODO description" -i http://iohk.io -t http://timestamp.verisign.com/scripts/timstamp.dll -tr 10 $FILE | cat
        storePath=$(nix-store --add-fixed sha256 $FILE)
        rm -rf $DIR
        echo $storePath >&3
      '';
      # requires --allow-unsafe-native-code-during-evaluation
      res = builtins.exec [ signingScript ];
    in res;
    signedCardano = pkgs.runCommand "signed-daedalus-bridge" {} ''
      cp -r ${self.unpackedCardano} $out
      chmod -R +w $out
      cd $out
      rm *.exe
      cp ${self.signFile "${self.unpackedCardano}/cardano-launcher.exe"} cardano-launcher.exe
      cp ${self.signFile "${self.unpackedCardano}/cardano-node.exe"} cardano-node.exe
      cp ${self.signFile "${self.unpackedCardano}/cardano-x509-certificates.exe"} cardano-x509-certificates.exe
      cp ${self.signFile "${self.unpackedCardano}/wallet-extractor.exe"} wallet-extractor.exe
    '';
    dummyUnpacked = pkgs.runCommand "dummy-unpacked-cardano" {} ''
      mkdir $out
      cd $out
      touch cardano-launcher.exe cardano-node.exe cardano-x509-certificates.exe log-config-prod.yaml configuration.yaml mainnet-genesis.json
    '';
    unpackedCardano = if dummyInstaller then self.dummyUnpacked else (if signingKeys != null then self.signedCardano else self.unsignedUnpackedCardano);
    nsisFiles = pkgs.runCommand "nsis-files" { buildInputs = [ old.daedalus-installer ]; } ''
      mkdir installers
      cp -vir ${./package.json} package.json
      cp -vir ${./installers/dhall} installers/dhall
      cd installers

      make-installer --os win64 -o $out --cluster ${cluster} buildkite-cross

      mkdir $out
      cp daedalus.nsi uninstaller.nsi launcher-config.yaml wallet-topology.yaml $out/
    '';
    uninstaller = pkgs.runCommand "uninstaller" { buildInputs = [ self.nsis pkgs.winePackages.minimal ]; } ''
      mkdir home
      export HOME=$(realpath home)

      ln -sv ${./installers/nsis_plugins} nsis_plugins
      cp ${self.nsisFiles}/uninstaller.nsi .

      makensis uninstaller.nsi -V4

      wine tempinstaller.exe /S
      mkdir $out
      mv -v $HOME/.wine/drive_c/uninstall.exe $out/uninstall.exe
    '';
    installer = pkgs.runCommand "win64-installer-${cluster}" { buildInputs = [ old.daedalus-installer self.nsis pkgs.unzip old.configMutator pkgs.jq old.yaml2json ]; } ''
      mkdir home
      export HOME=$(realpath home)

      mkdir -p $out/{nix-support,cfg-files}
      mkdir installers
      cp ${./cardano-sl-src.json} cardano-sl-src.json
      cp -vir ${./installers/dhall} installers/dhall
      cp -vir ${./installers/icons} installers/icons
      cp -vir ${./package.json} package.json
      chmod -R +w installers
      cd installers
      mkdir -pv ../release/win32-x64/
      ${if dummyInstaller then "mkdir -pv ../release/win32-x64/Daedalus-win32-x64/resources/app/dist/main/" else "cp -r ${old.rawapp-win64} ../release/win32-x64/Daedalus-win32-x64"}
      chmod -R +w ../release/win32-x64/Daedalus-win32-x64
      cp -v ${old.fastlist}/bin/fastlist.exe ../release/win32-x64/Daedalus-win32-x64/resources/app/dist/main/fastlist.exe
      ln -s ${./installers/nsis_plugins} nsis_plugins

      mkdir dlls
      pushd dlls
      ${if dummyInstaller then "touch foo" else "unzip ${self.dlls}"}
      popd
      cp -v ${self.unpackedCardano}/* .
      cp ${self.uninstaller}/uninstall.exe ../uninstall.exe
      cp -v ${self.nsisFiles}/{daedalus.nsi,wallet-topology.yaml,launcher-config.yaml} .
      chmod -R +w .
      ${lib.optionalString (fudgeConfig != null) ''
        set -x
        KEY=$(yaml2json launcher-config.yaml | jq .configuration.key -r)
        config-mutator configuration.yaml ''${KEY} ${toString fudgeConfig.applicationVersion} > temp
        mv temp configuration.yaml
        set +x
      ''}

      du -h

      makensis daedalus.nsi -V4

      cp daedalus-*-cardano-sl-*-windows.exe $out/
      cp *.yaml $out/cfg-files/
      echo file installer $out/*.exe > $out/nix-support/hydra-build-products
    '';
  };
in pkgs.lib.makeScope old.newScope packages
