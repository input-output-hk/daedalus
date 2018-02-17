let
  pkgs = import (import ./fetchNixpkgs.nix {
    rev = "c831224528cd6bfd49bfc2c18b9c5d9015651077";
    sha256 = "1idygi6x10wilrmj4w1djby8qi8zphxbp1zkzn4lmwhzfhgx2qnz";
  }) {
    config = {};
    overlays = [];
  };
  packages = self: {
    daedalus = self.callPackage ./linux.nix {};
    bundle = self.callPackage ./nix-bundle.nix {};
    bundle2 = (import ../nix-bundle { nixpkgs = pkgs; }).nix-bootstrap {
      target = "${self.example}";
      run = "/bin/example";
    };
    example = pkgs.writeScriptBin "example" ''
      #!${pkgs.stdenv.shell}
      echo original '$PATH' was '"'$PATH'"'
      export PATH=${pkgs.lib.concatMapStringsSep ":" (x: "${x}/bin/") (with pkgs; [ coreutils utillinux procps ])}
      mount
      pwd
      env
      ls -l / /home
    '';
  };
in pkgs.lib.makeScope pkgs.newScope packages
