#!/usr/bin/env bash
set -euo pipefail
script=$1
product="$PWD/fixture with spaces/Daedalus Pilot"
shared="$product/Shared"
framework='Frameworks/Electron Framework.framework'
mkdir -p "$shared/$framework/Versions/A/Resources" "$shared/MacOS/lib" "$shared/Resources/app/dist"
printf 'binary\n' > "$shared/$framework/Versions/A/Electron Framework"
printf 'icu\n' > "$shared/$framework/Versions/A/Resources/icudtl.dat"
ln -s A "$shared/$framework/Versions/Current"
ln -s Versions/Current/Resources "$shared/$framework/Resources"
printf 'backend\n' > "$shared/MacOS/cardano-node"
printf 'library\n' > "$shared/MacOS/lib/library.dylib"
printf 'frontend\n' > "$shared/MacOS/Frontend"
printf 'source launcher\n' > "$shared/MacOS/Daedalus Flight"
printf 'javascript\n' > "$shared/Resources/app/dist/index.js"
printf 'hidden\n' > "$shared/Resources/app/dist/.hidden"
printf 'source identity\n' > "$shared/Resources/app/package.json"
for name in 'Daedalus Preview' 'Daedalus Pre-Prod' 'Daedalus Flight'; do
  app="$product/$name.app/Contents"
  mkdir -p "$app/Frameworks/$name Helper.app/Contents" "$app/MacOS" "$app/Resources/app"
  printf '%s\n' "$name" > "$app/MacOS/$name"
  printf '%s\n' "$name" > "$app/Resources/app/package.json"
  printf '%s\n' "$name" > "$app/Frameworks/$name Helper.app/Contents/Info.plist"
  ln -s '../../../Shared/Frameworks/Electron Framework.framework' "$app/$framework"
  ln -s '../../../Shared/MacOS/lib' "$app/MacOS/lib"
  ln -s '../../../../Shared/Resources/app/dist' "$app/Resources/app/dist"
done
bash "$script" "$product"

# Binary caches and ordinary staging copies lose hard links. Reapply the same
# materialization step as the final pkg derivation, then relocate the result.
chmod -R a-w 'fixture with spaces'
cp -R 'fixture with spaces' 'staged with spaces'
chmod -R u+w 'staged with spaces'
product="$PWD/staged with spaces/Daedalus Pilot"
shared="$product/Shared"
test ! "$shared/MacOS/Frontend" -ef "$product/Daedalus Preview.app/Contents/MacOS/Frontend"
bash "$script" "$product" prune
for name in 'Daedalus Preview' 'Daedalus Pre-Prod' 'Daedalus Flight'; do
  app="$product/$name.app/Contents"
  test ! -e "$app/$framework"
  test ! -e "$app/MacOS/Frontend"
  test ! -e "$app/Resources/app/dist"
  test -f "$app/Resources/app/package.json"
  test -f "$app/MacOS/$name"
done
test -f "$shared/$framework/Resources/icudtl.dat"
bash "$script" "$product"
bash "$script" "$product"
mv 'staged with spaces' 'installed with spaces'
product="$PWD/installed with spaces/Daedalus Pilot"
shared="$product/Shared"
for name in 'Daedalus Preview' 'Daedalus Pre-Prod' 'Daedalus Flight'; do
  app="$product/$name.app/Contents"
  test ! -L "$app/$framework"
  test ! -L "$app/MacOS/lib"
  test ! -L "$app/Resources/app/dist"
  test -L "$app/$framework/Resources"
  test "$(readlink "$app/$framework/Versions/Current")" = A
  for file in "$framework/Versions/A/Electron Framework" "$framework/Resources/icudtl.dat" \
    MacOS/Frontend MacOS/cardano-node MacOS/lib/library.dylib Resources/app/dist/index.js Resources/app/dist/.hidden; do
    test "$shared/$file" -ef "$app/$file"
  done
  test "$(cat "$app/MacOS/$name")" = "$name"
  test "$(cat "$app/Resources/app/package.json")" = "$name"
  test "$(cat "$app/Frameworks/$name Helper.app/Contents/Info.plist")" = "$name"
done
