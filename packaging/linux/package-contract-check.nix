{
  deb,
  rpm,
  arch,
}: ''
  set -eu
  mkdir deb rpm arch control
  deb_file=$(printf '%s\n' ${deb}/*.deb)
  rpm_file=$(printf '%s\n' ${rpm}/*.rpm)
  arch_file=$(printf '%s\n' ${arch}/*.pkg.tar.zst)
  dpkg-deb --extract "$deb_file" deb
  dpkg-deb --control "$deb_file" control
  test "$(dpkg-deb --field "$deb_file" Package)" = daedalus-pilot
  test "$(dpkg-deb --field "$deb_file" Conflicts)" = 'daedalus-preview, daedalus-preprod, daedalus-mainnet-flight'
  test "$(rpm -qp --qf '%{NAME}' "$rpm_file")" = daedalus-pilot
  rpm -qp --conflicts "$rpm_file" > rpm-conflicts
  for profile in preview preprod mainnet-flight; do grep -Fx "daedalus-$profile" rpm-conflicts; done
  # Inspect package metadata for privileged modes; unprivileged extraction
  # deliberately omits them because Nix build users cannot set setuid.
  rpm -qp --qf '[%{FILENAMES} %{FILEMODES:perms}\n]' "$rpm_file" | \
    grep -Fx '/opt/daedalus/pilot/libexec/bundle-electron/lib/electron/chrome-sandbox -rwsr-xr-x'
  rpm2cpio "$rpm_file" | bsdtar --no-same-owner --no-same-permissions -xf - -C rpm
  bsdtar --no-same-owner --no-same-permissions -xf "$arch_file" -C arch
  for profile in preview preprod mainnet-flight; do
    grep -Fx "conflict = daedalus-$profile" arch/.PKGINFO
  done
  for format in deb rpm arch; do
    root="$format/opt/daedalus/pilot"
    test -f "$root/libexec/daedalus-js/main/dapp.js"
    test -f "$root/config/product.json"
    test ! -f "$root/config/daedalus-config.json"
    test -x "$root/bin/refuse-live"
    test ! -e "$format/opt/daedalus/mainnet"
    test ! -e "$format/usr/bin/daedalus-pilot"
    test "$(find "$root/libexec" -type f -name chrome-sandbox | wc -l)" -eq 1
    test "$(patchelf --print-interpreter "$root/libexec/bundle-electron/lib/electron/electron")" = /opt/daedalus/pilot/libexec/bundle-electron/lib/electron/ld-linux-x86-64.so.2
    for profile in preview preprod mainnet-flight; do
      test -x "$format/usr/bin/daedalus-$profile"
      test -x "$root/bin/daedalus-$profile"
      test -f "$root/config/profiles/$profile/daedalus-config.json"
      test ! -e "$format/opt/daedalus/$profile"
      desktop-file-validate "$format/usr/share/applications/Daedalus-$profile.desktop"
      grep -Fx "Exec=/opt/daedalus/pilot/bin/daedalus-$profile" "$format/usr/share/applications/Daedalus-$profile.desktop"
    done
  done
  grep -F 'refuse-live' control/preinst control/prerm
  rpm -qp --scripts "$rpm_file" | grep -F 'refuse-live'
  grep -Fx 'Target = daedalus-pilot' arch/usr/share/libalpm/hooks/daedalus-pilot-refuse-live.hook
  grep -F 'refuse-live' arch/usr/share/libalpm/scripts/daedalus-pilot-refuse-live
  touch "$out"
''
