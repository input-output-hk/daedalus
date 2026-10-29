# Shared by native packages and NixOS. Call after the common payload is installed,
# with $root and $desktop_root naming the payload and desktop staging trees.
{
  lib,
  profiles,
  daedalusConfigs,
  installRoot ? "/opt/daedalus/pilot",
  desktopPrefix ? "/usr",
  iconRoot ? "${desktopPrefix}/share/icons/hicolor/512x512/apps",
  sandboxHelper ? "${installRoot}/libexec/bundle-electron/lib/electron/chrome-sandbox",
  publicWrappers ? true,
  nativeMaintenance ? true,
  electronLaunch ? ''exec '${installRoot}/libexec/electron' --class="Daedalus-$CLUSTER" '${installRoot}/libexec/daedalus-js' "$@"'',
}: ''
  rm -f "$desktop_root${desktopPrefix}/bin/daedalus-pilot" \
    "$desktop_root${desktopPrefix}/share/applications/Daedalus-pilot.desktop" \
    "$desktop_root${desktopPrefix}/share/icons/hicolor/512x512/apps/daedalus-pilot.png"
  mkdir -p "$root/bin" "$desktop_root${desktopPrefix}/bin" \
    "$desktop_root${desktopPrefix}/share/applications" \
    "$desktop_root${desktopPrefix}/share/icons/hicolor/512x512/apps"
  cat >"$root/bin/daedalus" <<'PILOT_DISPATCH'
  #!/bin/sh
  set -eu
  case "''${1:-}" in
    ${lib.concatStringsSep "|" profiles}) profile=$1; shift ;;
    *) echo 'Choose Preview, Pre-Prod or Flight using its installed shortcut.' >&2; exit 2 ;;
  esac
  exec '${installRoot}/bin/daedalus-'"$profile" "$@"
  PILOT_DISPATCH
  chmod 0755 "$root/bin/daedalus"

  ${lib.concatMapStringsSep "\n" (profile: let
      configuration = daedalusConfigs.${profile};
      name = configuration.installerConfig.spacedName;
      icon = configuration.installerConfig.iconPath.base + "/512x512.png";
    in ''
      cat >"$root/bin/daedalus-${profile}" <<'PILOT_LAUNCH'
      #!/bin/sh
      set -eu
      unset LD_LIBRARY_PATH ENTRYPOINT_DIR
      ${lib.optionalString nativeMaintenance ''
        if [ -e '/var/lib/daedalus-package/pilot/removing' ]; then
          echo 'Daedalus Pilot package maintenance is in progress.' >&2
          exit 1
        fi
      ''}
      export CLUSTER='${profile}'
      export ENTRYPOINT_DIR='${installRoot}'
      export DAEDALUS_CONFIG='${installRoot}/config/profiles/${profile}'
      export CHROME_DEVEL_SANDBOX='${sandboxHelper}'
      export XDG_DATA_HOME="''${XDG_DATA_HOME:-''${HOME}/.local/share}"
      export XDG_CONFIG_HOME="''${XDG_CONFIG_HOME:-''${HOME}/.config}"
      export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"
      mkdir -p "''${DAEDALUS_DIR}/${profile}/Logs/pub" "''${DAEDALUS_DIR}/${profile}/Secrets"
      cd "''${DAEDALUS_DIR}/${profile}"
      exec '${installRoot}/libexec/cardano-watchdog' \
        --config '${installRoot}/config/profiles/${profile}/daedalus-config.json' \
        --pub-logs-dir "''${DAEDALUS_DIR}/${profile}/Logs/pub" \
        --tls-dir "''${DAEDALUS_DIR}/${profile}/tls"
      PILOT_LAUNCH
      chmod 0755 "$root/bin/daedalus-${profile}"
      ${lib.optionalString publicWrappers ''
        cat >"$desktop_root${desktopPrefix}/bin/daedalus-${profile}" <<'PILOT_WRAPPER'
        #!/bin/sh
        exec '${installRoot}/bin/daedalus-${profile}' "$@"
        PILOT_WRAPPER
        chmod 0755 "$desktop_root${desktopPrefix}/bin/daedalus-${profile}"
      ''}
      cat >"$desktop_root${desktopPrefix}/share/applications/Daedalus-${profile}.desktop" <<'PILOT_DESKTOP'
      [Desktop Entry]
      Type=Application
      Name=${name}
      GenericName=Crypto-Currency Wallet
      Exec=${installRoot}/bin/daedalus-${profile}
      Icon=daedalus-${profile}
      Categories=Network;Finance;
      StartupWMClass=Daedalus-${profile}
      Terminal=false
      PILOT_DESKTOP
      install -m 0644 ${icon} "$desktop_root${desktopPrefix}/share/icons/hicolor/512x512/apps/daedalus-${profile}.png"
    '')
    profiles}

  cat >"$root/libexec/daedalus-frontend" <<'PILOT_FRONTEND'
  #!/bin/sh
  set -eu
  case "''${CLUSTER:-}" in
    ${lib.concatStringsSep "|" profiles}) ;;
    *) echo 'Start Daedalus Pilot using an installed environment shortcut.' >&2; exit 2 ;;
  esac
  export DAEDALUS_WINDOW_ICON="${iconRoot}/daedalus-$CLUSTER.png"
  ${electronLaunch}
  PILOT_FRONTEND
  chmod 0755 "$root/libexec/daedalus-frontend"

  ${lib.optionalString nativeMaintenance ''
    cat >"$root/bin/refuse-live" <<'PILOT_LIFECYCLE'
    #!/bin/sh
    set -eu
    for proc_exe in /proc/[0-9]*/exe; do
      executable=$(readlink "$proc_exe" 2>/dev/null || true)
      case "$executable" in
        ${installRoot}/*)
          echo 'Close Preview, Pre-Prod and Flight before changing Daedalus Pilot.' >&2
          exit 1
          ;;
      esac
    done
    PILOT_LIFECYCLE
    chmod 0755 "$root/bin/refuse-live"
  ''}
''
