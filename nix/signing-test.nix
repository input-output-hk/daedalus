{ inputs, targetSystem }:

let pkgs = inputs.nixpkgs.legacyPackages.${targetSystem}; in

rec {

  inDerivation = pkgs.writeShellScriptBin "in" ''
    set -euo pipefail

    echo built ${pkgs.runCommandLocal "pure-wrapper" {} ''
      ${outsideOfDerivation}/bin/*
      touch $out
    ''}
  '';

  outsideOfDerivation = pkgs.writeShellScriptBin "outsideOfDerivation" ''
    set -euo pipefail
    export PATH="${pkgs.lib.makeBinPath [ pkgs.coreutils pkgs.jq ]}:$PATH"

    # XXX: if this is run inside the pure-wrapper, reuse the $out in /nix/store:
    if [ -z "''${out:-}" ] ; then export out="$(mktemp -d)" ; fi

    mkdir -p $out
    cd $out
    echo "We’re in $(pwd)"

    cp -v ${pkgs.darwin.libiconv}/lib/libiconv.2.4.0.dylib ./

    source /var/lib/buildkite-agent/signing.sh || true
    /usr/bin/security unlock-keychain -p "$KEYCHAIN" /var/lib/buildkite-agent/ci-signing.keychain-db || true
    PATH=$PATH:/usr/sbin

    # security import /var/root/bootstrap/signing/deps/iohk-codesign.cer -k /Library/Keychains/System.keychain

    cp ${entitlements} ./entitlements.xml

    set -x

    codeSigningIdentity="$(jq -r .codeSigningIdentity /var/lib/buildkite-agent/code-signing-config.json)"
    codeSigningKeyChain="$(jq -r .codeSigningKeyChain /var/lib/buildkite-agent/code-signing-config.json)"
    signingIdentity="$(jq -r .signingIdentity /var/lib/buildkite-agent/signing-config.json)"
    signingKeyChain="$(jq -r .signingKeyChain /var/lib/buildkite-agent/signing-config.json)"

    /usr/bin/codesign --force --verbose=4 --deep --strict --timestamp --options=runtime --entitlements ./entitlements.xml --sign "$codeSigningIdentity" *.dylib
  '';

  entitlements = pkgs.writeText "entitlements.xml" ''
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
      <dict>
        <key>com.apple.security.cs.allow-unsigned-executable-memory</key>
        <true/>
        <key>com.apple.security.cs.allow-dyld-environment-variables</key>
        <true/>
        <key>com.apple.security.cs.allow-jit</key>
        <true/>
      </dict>
    </plist>
  '';

}
