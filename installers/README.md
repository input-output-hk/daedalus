# Installers

This folder provides a way to create Daedalus installers for OSX/Linux/Windows.

Best way to see how to build an installer is to reproduce `../.buildkite/pipeline.yml` (OSX/Linux)
or `..\appveyor.yaml` (Windows).

## Certificate import on MacOS X

Before signing the Mac installer, the keychain needs to be set up. Do this by running:

    load-certificate -f macos.p12

The certificate is required to be in PKCS#12 format. It will prompt
for a certificate decryption password, or you can put this in the
`CERT_PASS` environment variable.
