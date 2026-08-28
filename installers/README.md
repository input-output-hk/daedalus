# Installers

This folder provides a way to create Daedalus installers for OSX/Linux/Windows.

Best way to see how to build an installer is to reproduce `../.buildkite/pipeline.yml`

## Certificate import on MacOS X

Before signing the Mac installer, the keychain needs to be set up. Do this by running:

    load-certificate -f macos.p12

The certificate is required to be in PKCS#12 format. It will prompt
for a certificate decryption password, or you can put this in the
`CERT_PASS` environment variable.

## Bumping the backend versions

The node and wallet the installer bundles are pinned as flake inputs in
[`flake.nix`](../flake.nix), with the resolved revisions in
[`flake.lock`](../flake.lock). To change either version, update its input
revision and run `nix flake lock --update-input cardano-node` (or
`cardano-wallet`).
