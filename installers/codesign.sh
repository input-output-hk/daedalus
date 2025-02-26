#!/usr/bin/env bash
set -x
SIGN_ID="$1"
KEYCHAIN="$2"
REL_PATH="$3"
XML_PATH="$4"
ABS_PATH="$(pwd)/$REL_PATH"
TS="$(date +%Y-%m-%d_%H-%M-%S)"
function sign_cmd() {
  for targetFile in "$@" ; do
    codesign --force --verbose=4 --deep --strict --timestamp --options=runtime --entitlements $XML_PATH --sign "$SIGN_ID" "$targetFile" 2>&1 | tee -a /tmp/codesign-output-${TS}.txt
  done
}
VERIFY_CMD="codesign --verbose=4 --verify --deep --strict"
ENTITLEMENT_CMD="codesign -d --entitlements :-"
LOG="2>&1 | tee -a /tmp/codesign-output-${TS}.txt"

# Remove symlinks pointing outside of the project build folder:
rm -f "$ABS_PATH/Contents/Resources/app/result"

# Ensure the code signing identity is found and set the keychain search path:
eval "security show-keychain-info \"$KEYCHAIN\" $LOG"
eval "security find-identity -v -p codesigning \"$KEYCHAIN\" $LOG"
eval "security list-keychains -d user -s \"$KEYCHAIN\" $LOG"

# Sign framework executables not signed by the deep sign command:
sign_cmd "$ABS_PATH/Contents/Frameworks/Squirrel.framework/Versions/A/Resources/ShipIt"
sign_cmd "$ABS_PATH/Contents/Frameworks/Electron Framework.framework/Versions/Current/Resources/crashpad_handler"
sign_cmd "$ABS_PATH/Contents/Frameworks/Electron Framework.framework/Versions/Current/Libraries/libnode.dylib"
sign_cmd "$ABS_PATH/Contents/Frameworks/Electron Framework.framework/Versions/Current/Libraries/libffmpeg.dylib"

sign_cmd "$ABS_PATH/Contents/Frameworks/Electron Framework.framework/Versions/A/Libraries/libEGL.dylib"
sign_cmd "$ABS_PATH/Contents/Frameworks/Electron Framework.framework/Versions/A/Libraries/libGLESv2.dylib"
sign_cmd "$ABS_PATH/Contents/Frameworks/Electron Framework.framework/Versions/A/Libraries/libswiftshader_libEGL.dylib"
sign_cmd "$ABS_PATH/Contents/Frameworks/Electron Framework.framework/Versions/A/Libraries/libswiftshader_libGLESv2.dylib"
sign_cmd "$ABS_PATH/Contents/Frameworks/Electron Framework.framework/Versions/A/Libraries/libvk_swiftshader.dylib"

# Sign the whole component deeply
sign_cmd "$ABS_PATH"

# Verify the signing
eval "$VERIFY_CMD \"$ABS_PATH\" $LOG"
eval "$VERIFY_CMD --display -r- \"$ABS_PATH\"" "$LOG"
eval "$ENTITLEMENT_CMD \"$ABS_PATH\"" "$LOG"
set +x
