#!/usr/bin/env bash
set -euo pipefail
script=$1
profiles=$2
postinstall=$3
verify=$4
shell=${5:-bash}
base="$PWD/base/Applications/Daedalus Pilot"
shared="$base/Shared"
framework='Frameworks/Electron Framework.framework'
mkdir -p "$shared/$framework/Versions/A/Resources" "$shared/MacOS/lib" "$shared/Resources/app/dist"
printf 'binary\n' > "$shared/$framework/Versions/A/Electron Framework"
printf 'icu\n' > "$shared/$framework/Versions/A/Resources/icudtl.dat"
ln -s A "$shared/$framework/Versions/Current"
ln -s Versions/Current/Resources "$shared/$framework/Resources"
printf 'backend\n' > "$shared/MacOS/cardano-node"
chmod +x "$shared/MacOS/cardano-node"
printf 'library\n' > "$shared/MacOS/lib/library.dylib"
printf 'frontend\n' > "$shared/MacOS/Frontend"
printf 'javascript\n' > "$shared/Resources/app/dist/index.js"
printf 'hidden\n' > "$shared/Resources/app/dist/.hidden"
cp "$profiles" "$base/pilot-layout.tsv"
chmod u+w "$base/pilot-layout.tsv"
# An extra, renamed profile catches hard-coded production names in the linker.
printf 'profile\tfixture\tRenamed Fixture\n' >> "$base/pilot-layout.tsv"
while IFS=$'\t' read -r _kind profile name; do
  app="$base/$name.app/Contents"
  mkdir -p "$app/Frameworks/$name Helper.app/Contents" "$app/MacOS" "$app/Resources/app"
  printf '%s\n' "$profile" > "$app/MacOS/$name"
  printf '%s\n' "$name" > "$app/Resources/app/package.json"
  printf '%s\n' "$name" > "$app/Frameworks/$name Helper.app/Contents/Info.plist"
done < "$base/pilot-layout.tsv"
for relative in MacOS/Frontend MacOS/cardano-node MacOS/lib "$framework" Resources/app/dist; do
  printf 'payload\t%s\n' "$relative" >> "$base/pilot-layout.tsv"
done
mkdir scripts
cp "$script" scripts/hardlink-payload
cp "$postinstall" scripts/postinstall
"$shell" -n scripts/postinstall

# Run the generated production wrapper with Installer arguments and spaces in
# the target path. No helper call substitutes for the real wrapper here.
cp -R base 'target volume'
bash "$verify" "$PWD/target volume/Applications/Daedalus Pilot" "$base" shells
"$shell" scripts/postinstall fixture.pkg / "$PWD/target volume"
bash "$verify" "$PWD/target volume/Applications/Daedalus Pilot" "$base"

# Probe must detect both a failing old cp and one that silently makes copies.
printf '#!/bin/sh\nexit 1\n' > broken-cp
cat > copying-cp <<'COPY'
#!/bin/sh
shift
exec cp -RP "$@"
COPY
chmod +x broken-cp copying-cp
for implementation in broken-cp copying-cp; do
  cp -R base "$implementation volume"
  product="$PWD/$implementation volume/Applications/Daedalus Pilot"
  "$shell" "$script" "$product" "$PWD/$implementation"
  bash "$verify" "$product" "$base"
  # A repeated materialization must preserve the result without overwriting it.
  "$shell" "$script" "$product" "$PWD/$implementation"
done

# A missing payload or conflicting private file must fail, preserving data.
cp -R base missing
product="$PWD/missing/Applications/Daedalus Pilot"
rm "$product/Shared/MacOS/cardano-node"
if "$shell" "$script" "$product"; then echo 'Accepted missing payload' >&2; exit 1; fi
cp -R base conflicting
product="$PWD/conflicting/Applications/Daedalus Pilot"
IFS=$'\t' read -r _kind _profile name < "$profiles"
printf 'private\n' > "$product/$name.app/Contents/MacOS/cardano-node"
if "$shell" "$script" "$product" "$PWD/broken-cp"; then echo 'Overwrote a conflict' >&2; exit 1; fi
test "$(cat "$product/$name.app/Contents/MacOS/cardano-node")" = private
