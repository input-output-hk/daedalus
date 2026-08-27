{inputs, ...}: {
  perSystem = {
    config,
    pkgs,
    inputs',
    ...
  }: {
    # This replaces the need for an external treefmt.toml
    treefmt = {
      # Point to the root of the repo
      projectRootFile = "flake.nix";

      # Enable alejandra for nix formatting
      programs.alejandra.enable = true;

      # Enable rustfmt for Rust — use the fenix toolchain rustfmt so it
      # matches the version used to build drt.
      programs.rustfmt = {
        enable = true;
        package = inputs'.fenix.packages.stable.rustfmt;
      };

      # Enable prettier for JS/TS/JSON/SCSS formatting.
      #
      # There is deliberately no `settings` block. treefmt-nix generates a
      # config file from `programs.prettier.settings` and passes it as
      # `--config`, which stops prettier discovering `.prettierrc`. With no
      # settings it emits no `--config` at all, so prettier reads `.prettierrc`
      # the same way `yarn prettier` does and the options have one source of
      # truth rather than two kept in step by hand.
      programs.prettier.enable = true;

      # Global settings and excludes
      settings.global.excludes = [
        "*.lock"
        "*.patch"
        "package-lock.json"
        "yarn.lock"
        ".gitattributes"
        ".gitignore"
        ".gitmodules"
        "LICENSE"
        # Exclude directories
        "node_modules"
        "dist"
        "release"
        ".direnv"
        ".agent"
        "release-cli/target"
        # Exclude specific paths from .prettierignore
        "source/renderer/app/i18n/locales"
        "source/renderer/app/config/newsfeed-files"
        "tests/paper-wallets/e2e/documents"
        "tests/wallets/e2e/documents"
      ];

      # Custom overrides for alejandra
      settings.formatter.alejandra = {
        includes = ["**/*.nix"];
      };

      # No prettier includes or excludes here on purpose. The include list is
      # controlled by `programs.prettier.includes`, which the treefmt-nix module
      # already defines; a list written under `settings.formatter.prettier` is a
      # second definition of the same option and concatenates with it rather
      # than replacing it, so a repository list can only ever be redundant.
      # Prettier's scope is decided by `.prettierignore`, which prettier applies
      # even to paths handed to it explicitly. The directory entries in
      # `settings.global.excludes` above do not exclude directory contents, so
      # they are not what is holding this line.
    };

    # This makes 'nix fmt' work automatically
    formatter = config.treefmt.build.wrapper;
  };
}
