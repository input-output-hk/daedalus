# Inlines: lib/windows-build.nix, lib/nsis.nix, lib/nsis-inner.nix, lib/fastlist.nix
# Provides _module.args.windowsBuild for x86_64-linux perSystem modules (cross-compile to Windows).
{inputs, ...}: {
  perSystem = {
    common,
    mkCommon,
    linuxBuild,
    pkgs,
    system,
    lib,
    ...
  }:
    lib.mkIf (system == "x86_64-linux") (let
      # common.nix with targetSystem="x86_64-windows" for wallet/node package selection,
      # but pkgs is x86_64-linux (actual build pkgs).
      commonWindows = mkCommon "x86_64-windows";

      # We use the linux common for build tools, but windows common for target packages:
      sourceLib = common.sourceLib;
      inherit (sourceLib) installerClusters;
      originalPackageJson = common.originalPackageJson;
      commonSources = common.commonSources;
      electronVersion = common.electronVersion;
      srcWithoutNix = common.srcWithoutNix;
      yarn = common.yarn;
      nodejs = common.nodejs;
      yarn2nix = common.yarn2nix;

      # Windows-target items from windows common:
      cardano-watchdog = commonWindows.cardano-watchdog;
      daedalus-bridge = commonWindows.daedalus-bridge;
      launcherConfigs = commonWindows.launcherConfigs;
      cardanoWalletVersion = commonWindows.cardanoWalletVersion;
      cardanoNodeVersion = commonWindows.cardanoNodeVersion;
      patchElectronRebuild = common.patchElectronRebuild;
      setupCacheAndGypDirs = common.setupCacheAndGypDirs;
      temporaryNodeModulesPatches = common.temporaryNodeModulesPatches;

      genClusters = lib.genAttrs installerClusters;

      # node_modules is reused from Linux build
      node_modules = linuxBuild.node_modules;

      # ---------------------------------------------------------------------------
      # Inlined fastlist.nix
      # ---------------------------------------------------------------------------
      fastlist = pkgs.pkgsCross.mingwW64.stdenv.mkDerivation {
        name = "fastlist";
        src = pkgs.fetchFromGitHub {
          owner = "MarkTiedemann";
          repo = "fastlist";
          rev = "65a9eaefa802fc4d3d3095f01a0321fd073a9098";
          sha256 = "0mw6x54n7bmi0fqw8drahcfk6yv232mymp476gy75h1sclk90fsa";
        };
        buildCommand = ''
          unpackPhase
          cd $sourceRoot
          mkdir -p $out/bin/
          $CC fastlist.cpp -o $out/bin/fastlist.exe
        '';
      };

      nsis = pkgs.nsis;

      wine = pkgs.wine.override {wineBuild = "wine32";};
      wine64 = pkgs.wine.override {wineBuild = "wineWow";};

      windowsSources = {
        electron = pkgs.fetchurl {
          url = "https://github.com/electron/electron/releases/download/v${electronVersion}/electron-v${electronVersion}-win32-x64.zip";
          hash = "sha256-CLYOSvnzmAm46Z8UA2pdFQhx9ejiXgLJV8Kg9U7M0Q0=";
        };

        # XXX: normally, node-gyp would download it only for Windows
        node-lib = pkgs.fetchurl {
          name = "node.lib-${electronVersion}";
          url = "https://electronjs.org/headers/v${electronVersion}/win-x64/node.lib";
          hash = "sha256-fmQN5y5hrEf2gbaf+Jc2PF7gEIixrSyAs9S62hu1v4g=";
        };
      };

      electron-cache = pkgs.runCommand "electron-cache" {} ''
        # newer style
        mkdir -p $out/${commonSources.electronCacheHash}/
        ln -sv ${windowsSources.electron} $out/${commonSources.electronCacheHash}/electron-v${electronVersion}-win32-x64.zip
        mkdir $out/httpsgithub.comelectronelectronreleasesdownloadv${electronVersion}electron-v${electronVersion}-win32-x64.zip
        ln -s ${windowsSources.electron} $out/httpsgithub.comelectronelectronreleasesdownloadv${electronVersion}electron-v${electronVersion}-win32-x64.zip/electron-v${electronVersion}-win32-x64.zip
      '';

      electronHeadersWithNodeLib =
        pkgs.runCommandLocal "electron-headers" {
          inherit (commonSources.electronHeaders) src;
        } ''
          mkdir $out
          tar -xf $src -C $out
          chmod -R +w $out
          echo 9 >$out/installVersion
          mkdir -p $out/Release
          ln -s ${windowsSources.node-lib} $out/Release/node.lib
        '';

      fresherPkgs = import (pkgs.fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs";
        rev = "17a689596b72d1906883484838eb1aaf51ab8001"; # nixos-unstable on 2023-05-15T08:29:41Z
        hash = "sha256-YPLMeYE+UzxxP0qbkBzv3RBDvyGR5I4d7v2n8dI3+fY=";
      }) {inherit (pkgs) system;};

      msvc-wine = pkgs.stdenv.mkDerivation {
        name = "msvc-wine";
        src = pkgs.fetchFromGitHub {
          owner = "mstorsjo";
          repo = "msvc-wine";
          rev = "c4fd83d53689f30ae6cfd8e9ef1ea01712907b59"; # 2023-05-09T21:52:05Z
          hash = "sha256-hA11dIOIL9sta+rwGb2EwWrEkRm6nvczpGmLZtr3nHI=";
        };
        buildInputs = [
          (pkgs.python3.withPackages (ps: with ps; [six]))
        ];
        configurePhase = ":";
        buildPhase = ":";
        installPhase = ''
          sed -r 's,msiextract,${pkgs.msitools}/bin/\0,g' -i vsdownload.py
          mkdir -p $out/libexec
          cp -r . $out/libexec/.
        '';
      };

      msvc-cache = let
        version = "16";
      in
        pkgs.stdenv.mkDerivation {
          name = "msvc-cache-${version}";
          inherit version;
          outputHashMode = "recursive";
          outputHashAlgo = "sha256";
          outputHash = "sha256-6RVneGKjsEMYGfZSYg5mMafjl0itdZQXtzLPRdrawII=";
          buildInputs = [];
          dontUnpack = true;
          dontConfigure = true;
          NIX_SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
          buildPhase = ''
            mkdir -p $out
            ${msvc-wine}/libexec/vsdownload.py --accept-license --major ${version} \
              --save-manifest \
              --only-download --cache $out --dest ./
            cp -v *.manifest $out/.
          '';
          dontInstall = true;
        };

      msvc-installed = pkgs.stdenv.mkDerivation {
        name = "msvc-installed-${msvc-cache.version}";
        inherit (msvc-cache) version;
        dontUnpack = true;
        dontConfigure = true;
        buildPhase = ''
          mkdir -p $out
          ${msvc-wine}/libexec/vsdownload.py --accept-license --major ${msvc-cache.version} \
            --manifest ${msvc-cache}/*.manifest \
            --keep-unpack --cache ${msvc-cache} --dest $out/
          mv $out/unpack/MSBuild $out/
        '';
        dontInstall = true;
      };

      native = rec {
        # XXX: Node.js 24 OOMs in V8 SegmentedTable subspace init under Wine 8.0.
        nativeNodejsVersion = "20.20.2";
        nodejs = pkgs.fetchzip {
          url = "https://nodejs.org/dist/v${nativeNodejsVersion}/node-v${nativeNodejsVersion}-win-x64.zip";
          hash = "sha256-uWoUlUXgejupc2ETcAoBTYcg65cMiNvvb8qseKodLCk=";
        };

        python = pkgs.fetchzip {
          url = "https://www.python.org/ftp/python/3.10.11/python-3.10.11-embed-amd64.zip";
          hash = "sha256-p83yidrRg5Rz1vQpyRuZCb5F+s3ddgHt+JakPjgFgUc=";
          stripRoot = false;
        };
      };

      nativeModules = pkgs.stdenv.mkDerivation {
        name = "daedalus-native-modules";
        src = common.srcLockfiles;
        nativeBuildInputs =
          [yarn nodejs]
          ++ (with fresherPkgs; [
            wineWowPackages.stableFull
            fontconfig
            winetricks
            samba
          ])
          ++ (with pkgs; [pkg-config jq file procps])
          ++ [pkgs.python3];
        buildInputs = with pkgs; [libusb1];
        configurePhase =
          setupCacheAndGypDirs
          + ''
            # Grab all cached `node_modules` from above:
            cp -r ${node_modules}/. ./
            chmod -R +w .
          '';
        FONTCONFIG_FILE = fresherPkgs.makeFontsCache {
          fontDirectories = with fresherPkgs; [
            dejavu_fonts
            freefont_ttf
            gyre-fonts
            liberation_ttf
            noto-fonts-emoji
            unifont
            winePackages.fonts
            xorg.fontcursormisc
            xorg.fontmiscmisc
          ];
        };
        buildPhase = let
          mkSection = title: ''
            echo ' '
            echo ' '
            echo ' '
            echo ' '
            echo ' '
            echo '===================== '${pkgs.lib.escapeShellArg title}' ====================='
          '';
          completeHack = "rebuild-complete-hack-bnlzMmdjbXB5emozNWFndGx1bnd5dnh5";
        in ''
          ${pkgs.xvfb-run}/bin/xvfb-run \
            --server-args="-screen 0 1920x1080x24 +extension GLX +extension RENDER -ac -noreset" \
            ${pkgs.writeShellScript "wine-setup-inside-xvfb" ''
            set -euo pipefail

            ${mkSection "Setting Windows system version"}
            winetricks -q win10

            ${mkSection "Setting up env and symlinks in standard locations"}

            # Symlink Windows SDK in a standard location:
            lx_program_files="$HOME/.wine/drive_c/Program Files (x86)"
            mkdir -p "$lx_program_files"
            ln -svf ${msvc-installed}/kits "$lx_program_files/Windows Kits"

            # Symlink VC in a standard location:
            vc_versionYear="$(jq -r .info.productLineVersion <${msvc-cache}/*.manifest)"
            lx_VSINSTALLDIR="$lx_program_files/Microsoft Visual Studio/$vc_versionYear/Community"
            mkdir -p "$lx_VSINSTALLDIR"
            ln -svf ${msvc-installed}/VC "$lx_VSINSTALLDIR"/
            ln -svf ${msvc-installed}/MSBuild "$lx_VSINSTALLDIR"/

            export VCINSTALLDIR="$(winepath -w "$lx_VSINSTALLDIR/VC")\\"
            export VCToolsVersion="$(ls ${msvc-installed}/VC/Tools/MSVC | head -n1)"
            export VCToolsInstallDir="$(winepath -w "$lx_VSINSTALLDIR/VC/Tools/MSVC/$VCToolsVersion")\\"
            export VCToolsRedistDir="$(winepath -w "$lx_VSINSTALLDIR/VC/Redist/MSVC/$VCToolsVersion")\\"

            export ClearDevCommandPromptEnvVars=false

            export VSINSTALLDIR="$(winepath -w "$lx_VSINSTALLDIR")\\"

            lx_WindowsSdkDir=("$lx_program_files/Windows Kits"/*)
            export WindowsSdkDir="$(winepath -w "$lx_WindowsSdkDir")\\"

            set -x

            wine reg ADD 'HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Microsoft SDKs\Windows\v10.0' \
              /v 'InstallationFolder' /t 'REG_SZ' /d "$WindowsSdkDir" /f

            wine reg ADD 'HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows Kits\Installed Roots' \
              /v 'KitsRoot10' /t 'REG_SZ' /d "$WindowsSdkDir" /f

            set +x

            ${mkSection "Preparing the 'info' structure"}
            jq --null-input \
              --arg msBuild      "$(winepath -w "$lx_VSINSTALLDIR/MSBuild/Current/Bin/MSBuild.exe")" \
              --arg path         "$VCINSTALLDIR" \
              --arg sdk          "$(ls ${msvc-installed}/kits/10/Include | head -n1)" \
              --arg toolset      "$(ls "$lx_VSINSTALLDIR/VC/Redist/MSVC" | grep -E '^v[0-9]+$')" \
              --arg version      "$(jq -r .info.productDisplayVersion <${msvc-cache}/*.manifest)" \
              --arg versionMajor "$(jq -r .info.productDisplayVersion <${msvc-cache}/*.manifest | cut -d. -f1)" \
              --arg versionMinor "$(jq -r .info.productDisplayVersion <${msvc-cache}/*.manifest | cut -d. -f2)" \
              --arg versionYear  "$(jq -r .info.productLineVersion    <${msvc-cache}/*.manifest)" \
              '{$msBuild,$path,$sdk,$toolset,$version,$versionMajor,$versionMinor,$versionYear}' \
              > vs-info.json

            ${mkSection "Stubbing node_modules/node-gyp/lib/find-visualstudio.js"}
            (
              cat <<<${pkgs.lib.escapeShellArg ''
              'use strict'
              async function findVisualStudio(nodeSemver, configMsvsVersion) {
                return (
            ''}
              cat vs-info.json
              cat <<<${pkgs.lib.escapeShellArg ''
                );
              }
              module.exports = { findVisualStudio }
            ''}
            ) >node_modules/node-gyp/lib/find-visualstudio.js
            cat node_modules/node-gyp/lib/find-visualstudio.js

            ${mkSection "Setting WINEPATH"}
            export WINEPATH="$(winepath -w ${native.nodejs});$(winepath -w ${native.python})"

            ${mkSection "Removing all symlinks to /nix/store (mostly python3)"}
            find node_modules -type l >all-symlinks.lst
            paste all-symlinks.lst <(xargs <all-symlinks.lst readlink) | grep -F /nix/store | cut -f1 | xargs rm -v
            rm all-symlinks.lst

            ${mkSection "Patching node_modules"}
            # Point electron-rebuild to the correct Node (Electron) headers location:
            ${patchElectronRebuild}/bin/* \
              "$(winepath -w ${electronHeadersWithNodeLib.src} | sed -r 's,\\,\\\\\\\\,g')" \
              "$(winepath -w ${electronHeadersWithNodeLib}     | sed -r 's,\\,\\\\\\\\,g')"

            ${mkSection "Running @electron/rebuild"}
            lx_electron_rebuild_bin="$(readlink -f node_modules/.bin/electron-rebuild)"
            export electron_rebuild_bin="$(winepath -w "$lx_electron_rebuild_bin")"

            # XXX: for some reason the build hangs (only on Cicero!) after outputting "Rebuild Complete":
            sed -r '/Rebuild Complete/a fs.writeFileSync("${completeHack}", "");' -i "$lx_electron_rebuild_bin"

            (
              while true ; do
                if [ -e ${completeHack} ] ; then
                  echo "Found ${completeHack}, killing node.exe among:"

                  ps aux | cat

                  pkill -9 node.exe || true
                  break
                else
                  sleep 2
                fi
              done
            ) &
            wine_killer_pid=$!

            wine ${native.nodejs}/node.exe "$electron_rebuild_bin" -f -w usb || {
              real_ec=$?
              if [ -e ${completeHack} ] ; then
                echo "Wine would return $real_ec, but ${completeHack} exists"
                exit 0
              else
                exit $real_ec
              fi
            }
            kill $wine_killer_pid || true

            # XXX: We're running in a separate namespace, so this is fine.
            while pgrep wine >/dev/null ; do
              ${mkSection "Wine is still running in the background, will try to kill it"}
              echo 'All remaining processes:'
              ps aux | cat

              sleep 1
              pkill -9 wine || true
              sleep 4
            done
          ''}
        '';
        installPhase = ''
          mkdir -p $out/build/Release
          cp node_modules/usb/build/Release/usb_bindings.node        $out/build/Release/
          cp node_modules/node-hid/build/Release/HID.node            $out/build/Release/

          # make sure they're for Windows
          find $out -iname '*.node' | while IFS= read -r ext ; do
            file "$ext" | grep -F 'MS Windows' || {
              echo "fatal: $ext is not built for MS Windows (shouldn't happen)"
              exit 2
            }
          done

          cp -r $out/build/Release $out/build/Debug
        '';
      };

      nativeModulesZip =
        pkgs.runCommand "win64-native-modules" {
          buildInputs = with pkgs; [zip];
        } ''
          mkdir -p $out

          (
            cd ${nativeModules}
            zip -ry $out/native-modules-${sourceLib.buildRevShort}-x86_64-windows.zip .
          )

          # Make it downloadable from Hydra:
          mkdir -p $out/nix-support
          echo "file binary-dist \"$(echo $out/*.zip)\"" >$out/nix-support/hydra-build-products
        '';

      windowsIcons = genClusters (cluster: let
        buildInputs = with pkgs; [imagemagick];
        srcCluster =
          if builtins.pathExists (../../../installers/icons + "/${cluster}")
          then cluster
          else "mainnet";
      in
        pkgs.runCommand "windows-icons-${cluster}" {inherit buildInputs;} ''
          mkdir -p $out/${cluster} $out
          cp -r ${../../../installers/icons + "/${srcCluster}"}/. $out/${cluster}/.
          cp ${../../../installers/icons/installBanner.bmp} $out/installBanner.bmp
          cd $out/${cluster}
          rm *.ico *.ICO || true
          for f in *.png ; do
            if [ "$f" == 1024x1024.png ] || [ "$f" == 512x512.png ] ; then continue ; fi
            convert "$f" "''${f%.png}.ico"
          done
          convert 16x16.png 24x24.png 32x32.png 48x48.png 64x64.png 128x128.png 256x256.png ${cluster}.ico
        '');

      nsisFiles = genClusters (cluster: let
        ic = launcherConfigs.${cluster}.installerConfig;
        ver = originalPackageJson.version;
        verParts = lib.splitString "." ver;
        viProductVersion =
          if builtins.length verParts == 4
          then ver
          else "0.0.0.0";
        versionMajor =
          if builtins.length verParts == 4
          then builtins.elemAt verParts 0
          else "0";
        versionMinor =
          if builtins.length verParts == 4
          then builtins.elemAt verParts 1
          else "0";
        packageFileName = "${ic.uglyName}-${ver}-${toString sourceLib.buildCounter}-${cluster}-${sourceLib.buildRevShort}-x86_64-windows.exe";

        uninstallerNsi = pkgs.writeText "uninstaller.nsi" ''
          Unicode true
          !addplugindir "nsis_plugins\liteFirewall\bin"
          SetCompress off
          Name "${ic.spacedName} Uninstaller ${ver}"
          OutFile "tempinstaller.exe"

          LoadLanguageFile "''${NSISDIR}\Contrib\Language files\English.nlf"
          LoadLanguageFile "''${NSISDIR}\Contrib\Language files\Japanese.nlf"

          Section ""
            SectionIn RO
            WriteUninstaller "c:\uninstall.exe"
          SectionEnd

          Section "un."
            DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${ic.spacedName}"
            DeleteRegKey HKLM "Software\${ic.spacedName}"
            RMDir /r /REBOOTOK "$INSTDIR"
            Delete "$SMPROGRAMS\${ic.spacedName}\*.*"
            Delete "$DESKTOP\${ic.spacedName}.lnk"
            liteFirewall::RemoveRule "$INSTDIR\cardano-node.exe" "Cardano Node"
            Pop $0
            DetailPrint "liteFirewall::RemoveRule: $0"
          SectionEnd
        '';

        installerNsi = pkgs.writeText "daedalus.nsi" ''
          Unicode true
          !define MUI_ICON "icons\${cluster}\${cluster}.ico"
          !define MUI_HEADERIMAGE
          !define MUI_HEADERIMAGE_BITMAP "icons\installBanner.bmp"
          !define MUI_HEADERIMAGE_RIGHT
          !include WinVer.nsh
          VIProductVersion ${viProductVersion}
          VIAddVersionKey "ProductVersion" "${ver}"
          RequestExecutionLevel highest
          !addplugindir "nsis_plugins\liteFirewall\bin"

          Name "${ic.spacedName} (${ver})"
          OutFile "${packageFileName}"

          InstallDir "$PROGRAMFILES64\${ic.spacedName}"
          InstallDirRegKey HKLM "Software\${ic.spacedName}" "Install_Dir"

          LoadLanguageFile "''${NSISDIR}\Contrib\Language files\English.nlf"
          LoadLanguageFile "''${NSISDIR}\Contrib\Language files\Japanese.nlf"

          LangString AlreadyRunning ''${LANG_ENGLISH} "is running. It needs to be fully shut down before running the installer!"
          LangString AlreadyRunning ''${LANG_JAPANESE} "が起動中です。 インストーラーを実行する前に完全にシャットダウンする必要があります！"
          LangString TooOld ''${LANG_ENGLISH} "This version of Windows is not supported. Windows 8.1 or above required."
          LangString TooOld ''${LANG_JAPANESE} "このWindowsバージョンはサポートされていません。Windows 8.1以降が必要です。"

          Var INSTALLEDAT

          Function .onInit
            ''${IfNot} ''${AtLeastWin8.1}
              MessageBox MB_OK "$(TooOld)"
              Quit
            ''${EndIf}
          FunctionEnd

          Function DirectoryPre
            ReadRegStr $INSTALLEDAT HKLM "Software\${ic.spacedName}" "Install_Dir"
            StrLen $R0 $INSTALLEDAT
            IntCmp $R0 0 +2
            Abort
          FunctionEnd

          Page directory DirectoryPre
          Page instfiles

          Section ""
            SectionIn RO
            SetOutPath "$INSTDIR"
            AllowSkipFiles off
            WriteRegStr HKLM "Software\${ic.spacedName}" "Install_Dir" "$INSTDIR"
            CreateDirectory "$APPDATA\${ic.installDirectory}\Secrets-1.0"
            CreateDirectory "$APPDATA\${ic.installDirectory}\Logs"
            CreateDirectory "$APPDATA\${ic.installDirectory}\Logs\pub"

            ; Wait up to 30 s for the app to release its lockfile before installing
            StrCpy $R0 0
            StrCpy $R1 "false"
          lockfileLoop:
            IntCmp $R0 30 lockfileDone 0 lockfileDone
            StrCmp $R1 "true" lockfileDone 0
            DetailPrint "Checking if ${ic.spacedName} is not running ($R0/30)..."
            StrCpy $R1 "true"
            ClearErrors
            Delete "$APPDATA\${ic.installDirectory}\${ic.uglyName}_lockfile"
            IfErrors lockfileNotDeleted lockfileDeleted
          lockfileNotDeleted:
            StrCpy $R1 "false"
          lockfileDeleted:
            StrCmp $R1 "true" lockfileSkipSleep 0
            Sleep 1000
          lockfileSkipSleep:
            IntOp $R0 $R0 + 1
            Goto lockfileLoop
          lockfileDone:
            StrCmp $R1 "true" lockfileOk 0
            Abort "${ic.installDirectory} $(AlreadyRunning)"
          lockfileOk:

            IfFileExists "$INSTDIR\*.*" 0 +2
              RMDir /r "$INSTDIR"

            IfFileExists "$APPDATA\${ic.installDirectory}\Wallet-1.0\open\*.*" 0 +2
              RMDir "$APPDATA\${ic.installDirectory}\Wallet-1.0\open"

            File "cardano-node.exe"
            File "cardano-wallet.exe"
            File "cardano-watchdog.exe"
            File "cardano-address.exe"
            File "cardano-cli.exe"
            File "mithril-client.exe"
            File "snapshot-converter.exe"
            File "config.yaml"
            File "topology.yaml"
            File "genesis.json"
            File /nonfatal "genesis-dijkstra.json"
            File /nonfatal "genesis-conway.json"
            File /nonfatal "checkpoints.json"
            File "genesis-byron.json"
            File "genesis-shelley.json"
            File "genesis-alonzo.json"
            File "peer-snapshot.json"
            File "libsodium-23.dll"
            File "libsecp256k1-2.dll"
            File "libffi-8.dll"
            File "libgmp-10.dll"
            File "libstdc++-6.dll"
            File "libmcfgthread-2.dll"
            File "libmcfgthread-minimal-2.dll"
            File "libgcc_s_seh-1.dll"
            File "zlib1.dll"
            File "libz.dll"
            File "libsnappy.dll"
            File "launcher-config.yaml"
            File "watchdog-config.json"
            File /r "..\release\win32-x64\${ic.spacedName}-win32-x64\"

            liteFirewall::AddRule "$INSTDIR\cardano-node.exe" "Cardano Node"
            Pop $0
            DetailPrint "liteFirewall::AddRule: $0"

            CreateShortcut "$DESKTOP\${ic.spacedName}.lnk" "$INSTDIR\cardano-watchdog.exe" "" "$INSTDIR\${ic.spacedName}.exe" 0 SW_SHOWMINIMIZED

            WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${ic.spacedName}" "InstallLocation" "$INSTDIR"
            WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${ic.spacedName}" "Publisher" "IOHK"
            WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${ic.spacedName}" "ProductVersion" "${ver}"
            WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${ic.spacedName}" "VersionMajor" "${versionMajor}"
            WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${ic.spacedName}" "VersionMinor" "${versionMinor}"
            WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${ic.spacedName}" "DisplayName" "${ic.spacedName}"
            WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${ic.spacedName}" "DisplayVersion" "${ver}"
            WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${ic.spacedName}" "UninstallString" "$\"$INSTDIR/uninstall.exe$\""
            WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${ic.spacedName}" "QuietUninstallString" "$\"$INSTDIR/uninstall.exe$\" /S"
            WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${ic.spacedName}" "NoModify" 1
            WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${ic.spacedName}" "NoRepair" 1
            File "uninstall.exe"
          SectionEnd

          Section "Start Menu Shortcuts"
            CreateDirectory "$SMPROGRAMS\${ic.spacedName}"
            CreateShortcut "$SMPROGRAMS\${ic.spacedName}\Uninstall ${ic.spacedName}.lnk" "$INSTDIR/uninstall.exe" "" "$INSTDIR/uninstall.exe" 0
            CreateShortcut "$SMPROGRAMS\${ic.spacedName}\${ic.spacedName}.lnk" "$INSTDIR\cardano-watchdog.exe" "" "$INSTDIR\${ic.installDirectory}.exe" 0 SW_SHOWMINIMIZED
          SectionEnd
        '';
      in
        pkgs.runCommand "nsis-files" {} ''
          mkdir $out
          cp ${uninstallerNsi} $out/uninstaller.nsi
          cp ${installerNsi} $out/daedalus.nsi
          cp -v ${launcherConfigs.${cluster}.configFiles}/* $out/
        '');

      unsignedUninstaller = genClusters (cluster:
        pkgs.runCommand "uninstaller" {buildInputs = [nsis wine];} ''
          mkdir home
          export HOME=$(realpath home)

          ln -sv ${../../../installers/nsis_plugins} nsis_plugins
          cp ${nsisFiles.${cluster}}/uninstaller.nsi .

          makensis uninstaller.nsi -V4

          wine tempinstaller.exe /S
          mkdir $out
          mv -v $HOME/.wine/drive_c/uninstall.exe $out/uninstall.exe
        '');

      preSigning = genClusters (cluster: let
        installDir = launcherConfigs.${cluster}.installerConfig.spacedName;
      in
        pkgs.runCommand "pre-signing" {buildInputs = [pkgs.unzip];} ''
          mkdir $out
          cd $out

          echo '~~~   Preparing files for installer'
          mkdir installers
          cp -vir ${windowsIcons.${cluster}} installers/icons
          cp -vir ${../../../package.json} package.json
          chmod -R +w installers
          cd installers
          mkdir -pv ../release/win32-x64/
          cp -rv ${daedalusJs.${cluster}} "../release/win32-x64/${installDir}-win32-x64"
          chmod -R +w "../release/win32-x64/${installDir}-win32-x64"
          cp -v ${fastlist}/bin/fastlist.exe "../release/win32-x64/${installDir}-win32-x64/resources/app/dist/main/fastlist.exe"
          ln -s ${../../../installers/nsis_plugins} nsis_plugins

          cp -vr ${daedalus-bridge.${cluster}}/bin/* .
          cp -v ${nsisFiles.${cluster}}/{*.yaml,*.json,daedalus.nsi,*.key,*.cert} .
          cp ${unsignedUninstaller.${cluster}}/uninstall.exe .
          if [ -f ${nsisFiles.${cluster}}/block-0.bin ]; then
            cp -v ${nsisFiles.${cluster}}/block-0.bin .
          fi
        '');

      makeInstaller = {
        signed ? false,
        cluster,
      }:
        pkgs.writeShellScriptBin "make-signed-installer" ''
          set -euo pipefail

          ${
            if signed
            then ''
              # We have to do it impurely:
              cd $(mktemp -d)
              echo "~~~ We're signing in $PWD:"

              sign_cmd() {
                echo "Signing: '$1'..."
                ssh "''${WIN_SIGN_HOST:-HSM}" <"$1" >"$1".signed
                mv "$1".signed "$1"
              }
            ''
            else ''
              sign_cmd() {
                echo "Would sign: '$1'"
              }
            ''
          }

          cp -r ${preSigning.${cluster}}/. ./
          chmod -R +w .

          find . '(' -iname '*.exe' -o -iname '*.dll' -o -iname '*.node' ')' | sort | while IFS= read -r binaryToSign ; do
            sign_cmd "$binaryToSign"
          done

          echo '~~~ Generating installer'
          (
            cd installers/
            ${nsis}/bin/makensis daedalus.nsi -V4
          )

          sign_cmd installers/daedalus-*-*.exe

          echo "Final installer: $(realpath installers/daedalus-*-*.exe)"
        '';

      daedalusJs = genClusters (cluster:
        pkgs.stdenv.mkDerivation {
          name = "daedalus-js";
          src = srcWithoutNix;
          nativeBuildInputs =
            [yarn nodejs wine64]
            ++ (with pkgs; [pkg-config unzip jq])
            ++ [pkgs.python3];
          buildInputs = [pkgs.libusb1];
          CARDANO_WALLET_VERSION = cardanoWalletVersion;
          CARDANO_NODE_VERSION = cardanoNodeVersion;
          CI = "nix";
          NETWORK = launcherConfigs.${cluster}.launcherConfig.networkName;
          BUILD_REV = sourceLib.buildRev;
          BUILD_REV_SHORT = sourceLib.buildRevShort;
          BUILD_COUNTER = sourceLib.buildCounter;
          NODE_ENV = "production";
          BUILDTYPE = "Release";
          configurePhase =
            setupCacheAndGypDirs
            + ''
              # Grab all cached `node_modules` from above:
              cp -r ${node_modules}/. ./
              chmod -R +w .
            '';
          patchedPackageJson = pkgs.writeText "package.json" (builtins.toJSON (
            pkgs.lib.recursiveUpdate originalPackageJson {
              productName = launcherConfigs.${cluster}.installerConfig.spacedName;
            }
          ));
          buildPhase = ''
            # Point electron-packager directly at the pre-fetched electron zip via --electron-zip-dir,
            # bypassing @electron/get's cache/download logic entirely.
            _electron_zip_dir=$(mktemp -d)
            ln -sf ${windowsSources.electron} "$_electron_zip_dir/electron-v${electronVersion}-win32-x64.zip"

            cp $patchedPackageJson package.json

            rm -r installers/icons/
            cp -r ${windowsIcons.${cluster}} installers/icons
            chmod -R +w installers/icons

            mkdir -p installers/icons/${cluster}/${cluster}
            cp ${windowsIcons.${cluster}}/${cluster}/* installers/icons/${cluster}/${cluster}/

            ${temporaryNodeModulesPatches}

            export DEBUG=electron-packager
            yarn --verbose --offline package --win64 --dir $(pwd) --icon installers/icons/${cluster}/${cluster} --electron-zip-dir "$_electron_zip_dir"
          '';
          installPhase = ''
            set -x

            ls -ltrh release/win32-x64/Daedalus*-win32-x64/
            cp -r release/win32-x64/Daedalus*-win32-x64 $out

            # XXX: nuke storepaths from map files
            (
              cd $out/resources/app/dist
              for x in {main,renderer}/{0.,}index.js{,.map} main/preload.js{,.map} main/0.js{,.map} renderer/styles.css.map; do
                ${pkgs.nukeReferences}/bin/nuke-refs $x
              done
            )

            rm -rf $out/resources/app/{installers,launcher-config.yaml,gulpfile.js,home}

            mkdir -pv $out/resources/app/node_modules
            jq -r '.[]' <${../../../packaging/runtime-nodejs-deps.json} | while IFS= read -r rtdep ; do
              mkdir -p "$(dirname "$out/resources/app/node_modules/$rtdep")"
              cp -r node_modules/"$rtdep" $out/resources/app/node_modules/"$rtdep"
            done

            chmod -R +w $out

            echo 'Deleting all redundant *.node files under to-be-distributed node_modules/:'
            (
              cd $out/
              find resources/ -name '*.node' -exec rm -vf '{}' ';'
              find resources/app/node_modules -type f '(' -name '*.o' -o -name '*.o.d' -o -name '*.target.mk' -o -name '*.Makefile' -o -name 'Makefile' -o -name 'config.gypi' ')' -exec rm -vf '{}' ';'
              sed -r 's#try: \[#\0 [process.env.DAEDALUS_INSTALL_DIRECTORY, "bindings"],#' -i resources/app/node_modules/bindings/bindings.js
            )

            # Place Windows-built native binaries where their respective loaders expect them.
            mkdir -p $out/resources/app/node_modules/node-hid/build/Release/
            cp ${nativeModules}/build/Release/HID.node $out/resources/app/node_modules/node-hid/build/Release/
            mkdir -p $out/resources/app/node_modules/usb/build/Release/
            cp ${nativeModules}/build/Release/usb_bindings.node $out/resources/app/node_modules/usb/build/Release/
          '';
          dontFixup = true;
        });

      windowsBuild = {
        inherit daedalus-bridge;

        package = preSigning;

        makeSignedInstaller = genClusters (cluster:
          makeInstaller {
            signed = true;
            inherit cluster;
          });

        unsignedInstaller = genClusters (cluster:
          pkgs.runCommand "win64-installer-${cluster}" {
            meta.mainProgram = "daedalus-${cluster}-installer";
          } ''
            ${makeInstaller {
              signed = false;
              inherit cluster;
            }}/bin/make-signed-installer
            mkdir $out
            cp -v installers/daedalus-*-*.exe $out/

            mkdir -p $out/bin
            ln $out/*.exe $out/bin/daedalus-${cluster}-installer

            mkdir -p $out/nix-support
            echo "file binary-dist \"$(echo $out/*.exe)\"" >$out/nix-support/hydra-build-products
          '');

        inherit nativeModulesZip;
      };
    in {
      _module.args.windowsBuild = windowsBuild;
    });
}
