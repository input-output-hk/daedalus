{ runCommandCC, daedalus_internal, electron, nukeReferences, xlibs, static_patchelf, libudev, nss }:

runCommandCC "daedalus-tarball" {
  buildInputs = [ nukeReferences ];
  allowedReferences = [ "out" ];
} ''
  mkdir -p $out/{bin,lib}
  cp ${./tarball-scripts/daedalus} $out/daedalus
  cp ${./tarball-scripts/check-install} $out/check-install

  cp ${electron}/lib/electron/.electron-wrapped $out/bin/electron
  cp ${static_patchelf}/bin/patchelf $out/bin/
  cp -L ${libudev.lib}/lib/libudev.so.1 $out/lib/
  cp -L ${nss}/lib/{libsoftokn3.so,libfreeblpriv3.so} $out/lib/

  for file in icudtl.dat snapshot_blob.bin natives_blob.bin locales content_resources_200_percent.pak pdf_viewer_resources.pak blink_image_resources_200_percent.pak views_resources_200_percent.pak resources content_shell.pak ui_resources_200_percent.pak; do
    cp -r ${electron}/lib/electron/$file $out/bin/
  done
  cp -L ${xlibs.libXScrnSaver}/lib/libXss.so.1 $out/lib/

  # mostly copied from nixpkgs/nixos/modules/system/boot/stage-1.nix

  # Copy all of the needed libraries
  find $out/bin $out/lib -type f | while read BIN; do
    echo "Copying libs for executable $BIN"
    LDD="$(ldd $BIN)" || continue
    LIBS="$(echo "$LDD" | awk '{print $3}' | sed '/^$/d')"
    for LIB in $LIBS; do
      TGT="$out/lib/$(basename $LIB)"
      if [ ! -f "$TGT" ]; then
        SRC="$(readlink -e $LIB)"
        cp -pdv "$SRC" "$TGT"
      fi
    done
  done

  chmod -R u+w $out

  find $out/bin $out/lib -type f | while read i; do
    if ! test -L $i; then
      nuke-refs -e $out $i
    fi
  done
''
