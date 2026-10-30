{
  pkgs,
  lib,
  profiles,
  configurations,
  version,
  filename,
}: let
  guard = prefix: ''
    Function ${prefix}CheckStopped
      ; Fail closed if process inspection fails. Include backends, not only UI.
      ClearErrors
      StrCpy $0 20
      ExecWait `$\"$SYSDIR\WindowsPowerShell\v1.0\powershell.exe$\" -NoProfile -NonInteractive -Command "try { $$prefix = '$INSTDIR\'; $$live = @(Get-CimInstance Win32_Process -ErrorAction Stop | Where-Object { $$_.ExecutablePath -and $$_.ExecutablePath.StartsWith($$prefix, [StringComparison]::OrdinalIgnoreCase) }); if ($$live.Count) { exit 10 }; exit 0 } catch { exit 20 }"` $0
      IfErrors inspectionFailed
      StrCmp $0 0 stopped
    inspectionFailed:
      MessageBox MB_OK|MB_ICONSTOP "Close Preview, Pre-Prod and Flight before changing Pilot. Process inspection must succeed before continuing."
      Abort
    stopped:
    FunctionEnd
  '';
  uninstaller = pkgs.writeText "pilot-uninstaller.nsi" ''
    Unicode true
    RequestExecutionLevel admin
    Name "Daedalus Pilot Uninstaller"
    !addplugindir "nsis_plugins\liteFirewall\bin"
    OutFile "tempinstaller.exe"
    SetCompress off
    ${guard "un."}
    Section
      WriteUninstaller "c:\uninstall.exe"
    SectionEnd
    Section "uninstall"
      SetShellVarContext current
      StrCmp $INSTDIR "$PROGRAMFILES64\Daedalus Pilot" correctRoot
      Abort "Unexpected Pilot installation directory."
    correctRoot:
      FileOpen $0 "$INSTDIR\pilot-owner" r
      IfErrors notOwned
      FileRead $0 $1
      FileClose $0
      StrCmp $1 "daedalus-pilot-v1$\r$\n" owned notOwned
    notOwned:
      Abort "Pilot ownership marker is missing or modified."
    owned:
      Call un.CheckStopped
      liteFirewall::RemoveRule "$INSTDIR\cardano-node.exe" "Daedalus Pilot Cardano Node"
      Pop $0
      ; Preserve the ownership marker and uninstaller until payload removal succeeds.
      ${lib.concatMapStringsSep "\n" (directory: ''
      RMDir /r "$INSTDIR\${directory}"
      IfFileExists "$INSTDIR\${directory}\*.*" 0 +2
      Abort "Pilot files remain in use. Close all profiles and retry removal."
    '') ["resources" "config" "icons" "locales" "swiftshader"]}
      Delete "$INSTDIR\cardano-*.exe"
      Delete "$INSTDIR\mithril-client.exe"
      Delete "$INSTDIR\snapshot-converter.exe"
      Delete "$INSTDIR\*.dll"
      Delete "$INSTDIR\Daedalus Pilot.exe"
      Delete "$INSTDIR\*.bin"
      Delete "$INSTDIR\*.pak"
      Delete "$INSTDIR\*.dat"
      Delete "$INSTDIR\*.json"
      Delete "$INSTDIR\LICENSE*"
      ${lib.concatMapStringsSep "\n" (pattern: ''
      IfFileExists "$INSTDIR\${pattern}" 0 +2
      Abort "Pilot files remain in use. Close all profiles and retry removal."
    '') ["cardano-*.exe" "mithril-client.exe" "snapshot-converter.exe" "*.dll" "*.bin" "*.pak" "*.dat" "*.json" "LICENSE*"]}
      IfFileExists "$INSTDIR\Daedalus Pilot.exe" 0 removed
      Abort "Pilot remains in use. Close all profiles and retry removal."
    removed:
      Delete "$INSTDIR\uninstall.exe"
      Delete "$INSTDIR\pilot-owner"
      RMDir "$INSTDIR"
      ${lib.concatMapStringsSep "\n" (profile: ''
        Delete "$DESKTOP\${configurations.${profile}.installerConfig.spacedName}.lnk"
      '')
      profiles}
      RMDir /r "$SMPROGRAMS\Daedalus Pilot"
      DeleteRegKey HKLM "Software\Daedalus Pilot"
      DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Daedalus Pilot"
    SectionEnd
  '';
  installer = pkgs.writeText "pilot-installer.nsi" ''
    Unicode true
    RequestExecutionLevel admin
    Name "Daedalus Pilot ${version} (test build)"
    !addplugindir "nsis_plugins\liteFirewall\bin"
    OutFile "${filename}"
    InstallDir "$PROGRAMFILES64\Daedalus Pilot"
    SetCompressor /SOLID lzma
    ${guard ""}
    Function .onInit
      SetShellVarContext current
      StrCpy $INSTDIR "$PROGRAMFILES64\Daedalus Pilot"
      ${lib.concatMapStringsSep "\n" (profile: ''
          ReadRegStr $0 HKLM "Software\${configurations.${profile}.installerConfig.spacedName}" "Install_Dir"
          StrCmp $0 "" noLegacy_${profile}
          MessageBox MB_OK|MB_ICONSTOP "Uninstall ${configurations.${profile}.installerConfig.spacedName} before installing Pilot. Preserve its wallet and chain data."
          Abort
        noLegacy_${profile}:
      '')
      profiles}
    FunctionEnd
    Page instfiles
    Section
      Call CheckStopped
      ; Until native upgrade/rollback acceptance passes, require clean install.
      IfFileExists "$INSTDIR\*.*" 0 fresh
      Abort "Uninstall the existing Pilot installation before installing this test build. User data is retained."
    fresh:
      SetOutPath "$INSTDIR"
      AllowSkipFiles off
      File "uninstall.exe"
      FileOpen $0 "$INSTDIR\pilot-owner" w
      FileWrite $0 "daedalus-pilot-v1$\r$\n"
      FileClose $0
      ${lib.concatMapStringsSep "\n" (name: ''File "${name}.exe"'') ["cardano-node" "cardano-wallet" "cardano-watchdog" "cardano-address" "cardano-cli" "mithril-client" "snapshot-converter"]}
      File /nonfatal "*.dll"
      File /r "..\release\win32-x64\Daedalus Pilot-win32-x64\"
      SetOutPath "$INSTDIR\config"
      File /r "config\*"
      SetOutPath "$INSTDIR\icons"
      File /r "profile-icons\*"
      SetOutPath "$INSTDIR"
      liteFirewall::AddRule "$INSTDIR\cardano-node.exe" "Daedalus Pilot Cardano Node"
      Pop $0
      WriteRegStr HKLM "Software\Daedalus Pilot" "Install_Dir" "$INSTDIR"
      WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Daedalus Pilot" "DisplayName" "Daedalus Pilot"
      WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Daedalus Pilot" "DisplayVersion" "${version}"
      WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Daedalus Pilot" "UninstallString" '$\"$INSTDIR\uninstall.exe$\"'
      CreateDirectory "$SMPROGRAMS\Daedalus Pilot"
      CreateShortcut "$SMPROGRAMS\Daedalus Pilot\Uninstall Daedalus Pilot.lnk" "$INSTDIR\uninstall.exe"
      ${lib.concatMapStringsSep "\n" (profile: let
        name = configurations.${profile}.installerConfig.spacedName;
      in ''
        CreateShortcut "$DESKTOP\${name}.lnk" "$INSTDIR\cardano-watchdog.exe" "--config $\"$INSTDIR\config\profiles\${profile}\daedalus-config.json$\"" "$INSTDIR\icons\${profile}.ico" 0 SW_SHOWMINIMIZED
        CreateShortcut "$SMPROGRAMS\Daedalus Pilot\${name}.lnk" "$INSTDIR\cardano-watchdog.exe" "--config $\"$INSTDIR\config\profiles\${profile}\daedalus-config.json$\"" "$INSTDIR\icons\${profile}.ico" 0 SW_SHOWMINIMIZED
      '')
      profiles}
    SectionEnd
  '';
in
  pkgs.runCommand "pilot-windows-configs-and-nsis" {} ''
    mkdir -p "$out/config/profiles"
    cp ${installer} "$out/daedalus.nsi"
    cp ${uninstaller} "$out/uninstaller.nsi"
    ${lib.concatMapStringsSep "\n" (profile: let
        name = configurations.${profile}.installerConfig.spacedName;
        appId = "org.daedalus.pilot.${profile}";
      in ''
        destination="$out/config/profiles/${profile}"
        mkdir -p "$destination/frontend"
        cp -r ${configurations.${profile}.configFiles}/. "$destination/"
        cat > "$destination/frontend/package.json" <<'METADATA'
        ${builtins.toJSON {
          productName = name;
          inherit version;
          main = "bootstrap.cjs";
        }}
        METADATA
        cat > "$destination/frontend/bootstrap.cjs" <<'BOOTSTRAP'
        const { app, shell } = require('electron');
        const fs = require('fs');
        const path = require('path');
        app.setAppUserModelId('${appId}');
        const root = path.dirname(process.resourcesPath);
        const icon = path.join(root, 'icons', '${profile}.ico');
        const config = path.join(root, 'config', 'profiles', '${profile}', 'daedalus-config.json');
        app.on('browser-window-created', (_event, window) => {
          window.setIcon(icon);
          window.setAppDetails({
            appId: '${appId}', appIconPath: icon, appIconIndex: 0,
            relaunchDisplayName: '${name}',
            relaunchCommand: '"' + path.join(root, 'cardano-watchdog.exe') + '" --config "' + config + '"'
          });
        });
        for (const shortcut of [
          path.join(app.getPath('desktop'), '${name}.lnk'),
          path.join(app.getPath('appData'), 'Microsoft', 'Windows', 'Start Menu', 'Programs', 'Daedalus Pilot', '${name}.lnk')
        ]) {
          if (fs.existsSync(shortcut)) shell.writeShortcutLink(shortcut, 'update', {appUserModelId: '${appId}'});
        }
        require(path.join(process.resourcesPath, 'shared-app', 'dist', 'main', 'index.js'));
        BOOTSTRAP
      '')
      profiles}
  ''
