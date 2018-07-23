{ lib, stdenv, runCommand, fetchurl, squashfsTools }:

let
  AppImageKit = {
    AppRun = fetchurl {
      url = "https://github.com/AppImage/AppImageKit/releases/download/10/AppRun-x86_64";
      sha256 = "0fcqy19083896j78p57vi9wcdgkyprgnjpx0slmv0c37bg12b1a9";
    };
    runtime = fetchurl {
      url = "https://github.com/AppImage/AppImageKit/releases/download/10/runtime-x86_64";
      sha256 = "13aiwyd5pmpkha5ccyfragxqf8ymrbg0vsv53lfy4pb36yadvbzr";
    };
  };

in

  { name
  , buildCommand
  , ...
  } @ attrs:

  let
    unpacked = stdenv.mkDerivation ({
      name = "${name}.AppDir";
      buildCommand = ''
        ${buildCommand}
        mkdir -p $out
        cp ${AppImageKit.AppRun} $out/AppRun
        chmod +x $out/AppRun
        chmod -R +w $out
      '';
    } // removeAttrs attrs ["name" "buildCommand"]);

    appImage = stdenv.mkDerivation ({
      name = "${name}.AppImage";
      passthru = { inherit unpacked; } // attrs.passthru or {};
      buildCommand = ''
        ${squashfsTools}/bin/mksquashfs ${unpacked} ${name}.squashfs -root-owned -noappend
        cat ${AppImageKit.runtime} >> $out
        cat ${name}.squashfs >> $out
        chmod a+x $out
      '';
    } // removeAttrs attrs ["name" "passthru" "buildCommand"]);

  in
    appImage
