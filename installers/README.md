# Installers

This folder provides a way to create Daedalus installers for OSX/Linux/Windows.

Best way to see how to build an installer is to reproduce `../.buildkite/pipeline.yml`

## Certificate import on MacOS X

Before signing the Mac installer, the keychain needs to be set up. Do this by running:

    load-certificate -f macos.p12

The certificate is required to be in PKCS#12 format. It will prompt
for a certificate decryption password, or you can put this in the
`CERT_PASS` environment variable.

## Bumping cardano-sl version

The cardano-sl node and configuration files used by the installer
builder are available as the `daedalus-bridge` attribute of the
top-level file [`default.nix`](../default.nix). To get/build the
files, run:

    nix-build -A daedalus-bridge
    ls result/

To update the cardano-sl version, update the revision in
[`cardano-sl-src.json`](../cardano-sl-src.json). This replaces the
`CARDANO_SL_BRANCH` environment variable which was previously used.
