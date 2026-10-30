#!/usr/bin/env bash
set -euo pipefail

layout_script=$1
product="$PWD/fixture with spaces/Daedalus Pilot"
shared="$product/Shared"
mkdir -p "$shared/Frameworks/Electron Framework.framework/Versions/A"
printf 'one shared framework\n' > "$shared/Frameworks/Electron Framework.framework/Versions/A/Electron Framework"
for suffix in '' ' (Renderer)' ' (GPU)' ' (Plugin)'; do
  helper="$shared/Frameworks/Daedalus Flight Helper$suffix.app/Contents"
  mkdir -p "$helper/MacOS" "$helper/_CodeSignature"
  printf '#!%s\nexit 0\n' "$(command -v bash)" > "$helper/MacOS/Daedalus Flight Helper$suffix"
  chmod +x "$helper/MacOS/Daedalus Flight Helper$suffix"
  printf '{"CFBundleIdentifier":"old", "CFBundleName":"old", "CFBundleExecutable":"old"}\n' > "$helper/Info.plist"
done

# PlistBuddy is macOS-only. This fixture records its key edits as JSON; the
# production script still uses native PlistBuddy against real Info.plist files.
cat > plist-buddy-fixture <<'BUDDY'
#!/usr/bin/env bash
set -eu
test "$1" = -c
command=$2
file=$3
case "$command" in
  'Set :'*) command=${command#Set :} ;;
  'Add :'*) command=${command#Add :} ;;
  *) exit 1 ;;
esac
key=${command%% *}
value=${command#* }
value=${value#string }
jq --arg key "$key" --arg value "$value" '.[$key] = $value' "$file" > "$file.tmp"
mv "$file.tmp" "$file"
BUDDY
sed -i "1c #!$(command -v bash)" plist-buddy-fixture
chmod +x plist-buddy-fixture

for profile in preview preprod mainnet-flight; do
  case "$profile" in
    preview) name='Daedalus Preview' ;;
    preprod) name='Daedalus Pre-Prod' ;;
    mainnet-flight) name='Daedalus Flight' ;;
  esac
  app="$product/$name.app/Contents"
  bash "$layout_script" "$shared" "$app" "$name" "org.daedalus.pilot.$profile" 'Daedalus Flight' "$PWD/plist-buddy-fixture"
done

# Simulate staging/installation at a different root. Relative framework links
# must still work, while helper executables must resolve inside their own app.
mv 'fixture with spaces' 'installed with spaces'
product="$PWD/installed with spaces/Daedalus Pilot"
for profile in preview preprod mainnet-flight; do
  case "$profile" in
    preview) name='Daedalus Preview' ;;
    preprod) name='Daedalus Pre-Prod' ;;
    mainnet-flight) name='Daedalus Flight' ;;
  esac
  app="$product/$name.app/Contents"
  test -d "$app/Frameworks" && test ! -L "$app/Frameworks"
  test -L "$app/Frameworks/Electron Framework.framework"
  test "$(cat "$app/Frameworks/Electron Framework.framework/Versions/A/Electron Framework")" = 'one shared framework'
  for suffix in '' ' (Renderer)' ' (GPU)' ' (Plugin)'; do
    helper="$name Helper$suffix"
    bundle="$app/Frameworks/$helper.app/Contents"
    executable="$bundle/MacOS/$helper"
    test "$(realpath "$executable")" = "$executable"
    "$executable"
    test ! -e "$bundle/_CodeSignature"
    jq -e --arg name "$helper" --arg id "org.daedalus.pilot.$profile.helper" '
      .CFBundleName == $name and .CFBundleExecutable == $name and
      .CFBundleDisplayName == $name and (.CFBundleIdentifier | startswith($id))
    ' "$bundle/Info.plist"
  done
done
