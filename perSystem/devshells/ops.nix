{inputs, ...}: {
  perSystem = {
    config,
    lib,
    pkgs,
    ...
  }: let
    cargoLockExists = builtins.pathExists ./../../release-cli/Cargo.lock;

    shellHook = ''
      bold='\033[1m'
      cyan='\033[1;36m'
      yellow='\033[1;33m'
      green='\033[1;32m'
      dim='\033[2m'
      reset='\033[0m'
      printf "\n"
      printf "''${cyan}  ██████╗  █████╗ ███████╗██████╗  █████╗ ██╗     ██╗   ██╗███████╗\n"
      printf "  ██╔══██╗██╔══██╗██╔════╝██╔══██╗██╔══██╗██║     ██║   ██║██╔════╝\n"
      printf "  ██║  ██║███████║█████╗  ██║  ██║███████║██║     ██║   ██║███████╗\n"
      printf "  ██║  ██║██╔══██║██╔══╝  ██║  ██║██╔══██║██║     ██║   ██║╚════██║\n"
      printf "  ██████╔╝██║  ██║███████╗██████╔╝██║  ██║███████╗╚██████╔╝███████║\n"
      printf "  ╚═════╝ ╚═╝  ╚═╝╚══════╝╚═════╝ ╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚══════╝''${reset}\n"
      printf "''${yellow}  ██████╗ ███████╗██╗     ███████╗ █████╗ ███████╗███████╗\n"
      printf "  ██╔══██╗██╔════╝██║     ██╔════╝██╔══██╗██╔════╝██╔════╝\n"
      printf "  ██████╔╝█████╗  ██║     █████╗  ███████║███████╗█████╗\n"
      printf "  ██╔══██╗██╔══╝  ██║     ██╔══╝  ██╔══██║╚════██║██╔══╝\n"
      printf "  ██║  ██║███████╗███████╗███████╗██║  ██║███████║███████╗\n"
      printf "  ╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝╚═╝  ╚═╝╚══════╝╚══════╝''${reset}\n"
      printf "''${green}  ████████╗ ██████╗  ██████╗ ██╗\n"
      printf "  ╚══██╔══╝██╔═══██╗██╔═══██╗██║\n"
      printf "     ██║   ██║   ██║██║   ██║██║\n"
      printf "     ██║   ██║   ██║██║   ██║██║\n"
      printf "     ██║   ╚██████╔╝╚██████╔╝███████╗\n"
      printf "     ╚═╝    ╚═════╝  ╚═════╝ ╚══════╝''${reset}''${dim}  ops shell''${reset}\n"
      printf "\n"
      printf "  ''${bold}$(drt --version)''${reset}\n"
      printf "\n"
      printf "  ''${yellow}Workflow''${reset}\n"
      printf "  ''${green}1. fetch''${reset}    drt fetch-installers --url https://ci.iog.io/eval/<ID> --env <cluster> -o installers/<cluster>/\n"
      printf "  ''${green}2. sign''${reset}     drt sign installers/<cluster>/\n"
      printf "  ''${dim}            OSX_SIGN_HOST  WIN_SIGN_HOST  GPG_USER from .envrc.local''${reset}\n"
      printf "  ''${green}3. serve''${reset}    drt serve --installers installers/<cluster>/\n"
      printf "  ''${dim}            test locally before releasing''${reset}\n"
      printf "  ''${green}4. release''${reset}  drt release installers/<cluster>/ --bucket <bucket> --bucket-url <url>\n"
      printf "\n"
    '';
  in
    lib.optionalAttrs cargoLockExists {
      devShells.ops = pkgs.mkShell {
        packages = [config.packages.drt pkgs.gnupg];
        inherit shellHook;
      };
    };
}
