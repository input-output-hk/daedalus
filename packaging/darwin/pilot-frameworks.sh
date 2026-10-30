#!/usr/bin/env bash
set -euo pipefail

shared=$1
app=$2
name=$3
bundle_id=$4
source_name=$5
plist_buddy=${6:-/usr/libexec/PlistBuddy}

# Helpers must be real, profile-named bundles below this app's Contents:
# Electron derives both its helper name and parent app from that location.
# Keep the large framework binaries in the shared payload.
mkdir -p "$app/Frameworks"
for item in "$shared/Frameworks"/*; do
  base=$(basename "$item")
  case "$base" in
    "$source_name Helper"*.app)
      source_helper=${base%.app}
      suffix=${source_helper#"$source_name Helper"}
      helper="$name Helper$suffix"
      destination="$app/Frameworks/$helper.app/Contents"
      cp -R "$item" "$app/Frameworks/$helper.app"
      chmod -R u+w "$destination"
      rm -rf "$destination/_CodeSignature"
      if [ "$source_helper" != "$helper" ]; then
        mv "$destination/MacOS/$source_helper" "$destination/MacOS/$helper"
      fi
      test -x "$destination/MacOS/$helper"
      test "$(wc -c < "$destination/MacOS/$helper")" -lt 2097152
      helper_id=$(printf '%s' "$suffix" | tr -d ' ()' | tr '[:upper:]' '[:lower:]')
      helper_id="$bundle_id.helper${helper_id:+.$helper_id}"
      "$plist_buddy" -c "Set :CFBundleIdentifier $helper_id" "$destination/Info.plist"
      "$plist_buddy" -c "Set :CFBundleName $helper" "$destination/Info.plist"
      "$plist_buddy" -c "Set :CFBundleDisplayName $helper" "$destination/Info.plist" || \
        "$plist_buddy" -c "Add :CFBundleDisplayName string $helper" "$destination/Info.plist"
      "$plist_buddy" -c "Set :CFBundleExecutable $helper" "$destination/Info.plist"
      ;;
    *) ln -s "../../../Shared/Frameworks/$base" "$app/Frameworks/$base" ;;
  esac
done

# These are the four subprocess paths used by the pinned Electron runtime.
for suffix in '' ' (Renderer)' ' (GPU)' ' (Plugin)'; do
  helper="$name Helper$suffix"
  test -x "$app/Frameworks/$helper.app/Contents/MacOS/$helper"
done
