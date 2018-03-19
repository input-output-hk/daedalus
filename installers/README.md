# Installers

This folder provides a way to create Daedalus installers for OSX/Linux/Windows.

Best way to see how to build an installer is to reproduce `../.buildkite/pipeline.yml` (OSX/Linux)
or `..\appveyor.yaml` (Windows).

## Generation of runtime configuration files

The run-time configuration files of the Daedalus wallet are generated during the
installer build, and are governed by configuration files written in the Dhall
configuration language, which are factored per OS and target cluster:

  - https://github.com/input-output-hk/daedalus/tree/develop/installers/dhall

The Dhall expressions that comprise the runtime configuration are thus composed from:
  - `launcher.dhall` -- top level expression defining the launcher configuration YAML file
  - `topology.dhall` -- top level expression defining the wallet topology YAML file
  - `{linux,macos64,win64}.dhall`
  - `{mainnet,staging}.dhall`

The set of clusters (currently `mainnet` and `staging`) that the build scripts
(`scripts/build-installer-*`) will build installers for is enumerated in
https://github.com/input-output-hk/daedalus/blob/develop/installer-clusters.cfg

### Validating the Dhall configuration files

The entire set of configuration files can be validated by typechecking all
possible combinations:

    $ cd installers
    $ ghci Config.hs
    GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
    Loaded GHCi configuration from /home/deepfire/.ghci
    [1 of 2] Compiling Types            ( Types.hs, interpreted )
    [2 of 2] Compiling Config           ( Config.hs, interpreted )
    Ok, modules loaded: Config, Types.
    *Config> generateAllConfigs "./dhall"

..which will be silent if everything is well, and will otherwise provide
a detailed explanation of what is wrong, and where.

## Certificate import on MacOS X

Before signing the Mac installer, the keychain needs to be set up. Do this by running:

    load-certificate -f macos.p12

The certificate is required to be in PKCS#12 format. It will prompt
for a certificate decryption password, or you can put this in the
`CERT_PASS` environment variable.

## Bumping cardano-sl version

The cardano-sl node and configuration files used by the installer
builder are available as the `daedalus-bridge` attribute of the
top-level file [`cardano-sl.nix`](../cardano-sl.nix). To get/build the
files, run:

    nix-build cardano-sl.nix -A daedalus-bridge
    ls result/

To update the cardano-sl version, update the revision in
[`cardano-sl-src.json`](../cardano-sl-src.json). This replaces the
`CARDANO_SL_BRANCH` environment variable which was previously used.
