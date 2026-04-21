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

      # Enable prettier for JS/TS/JSON/SCSS formatting
      programs.prettier = {
        enable = true;
        # Use the settings from .prettierrc
        settings = {
          trailingComma = "es5";
          singleQuote = true;
        };
      };

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

      # Custom overrides for prettier - match .prettierignore includes
      settings.formatter.prettier = {
        includes = [
          "source/**/*.js"
          "source/**/*.ts"
          "source/**/*.tsx"
          "source/**/*.scss"
          "source/**/*.json"
          "features/**/*.js"
          "features/**/*.ts"
          "features/**/*.tsx"
          "features/**/*.scss"
          "features/**/*.json"
          "storybook/**/*.js"
          "storybook/**/*.ts"
          "storybook/**/*.tsx"
          "storybook/**/*.scss"
          "storybook/**/*.json"
          "hardware-wallet-tests/**/*.js"
          "hardware-wallet-tests/**/*.ts"
          "hardware-wallet-tests/**/*.tsx"
          "hardware-wallet-tests/**/*.json"
          "tests/**/*.js"
          "tests/**/*.ts"
          "tests/**/*.tsx"
          "tests/**/*.scss"
          "tests/**/*.json"
          "package.json"
        ];
        excludes = [
          "source/renderer/app/i18n/locales/**"
          "source/renderer/app/config/newsfeed-files/**"
          "tests/paper-wallets/e2e/documents/**"
          "tests/wallets/e2e/documents/**"
        ];
      };
    };

    # This makes 'nix fmt' work automatically
    formatter = config.treefmt.build.wrapper;
  };
}
