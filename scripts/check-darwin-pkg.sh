#!/usr/bin/env bash

set -o errexit
set -o pipefail

# xar -xf ../daedalus-4.10.0-testnet-0.pkg     #  0.487 s total
# cd daedalus-4.10.0-testnet-0.pkg
# cat Payload| gunzip | cpio -i                #  7.712 s total
# cd Daedalus\ Testnet.app/Contents/

#echo 'Finding all binaries…'

allBinaries=$(find . -type f -not '(' -name '*.js' -o -name '*.ts' -o -name '*.ts.map' -o -name '*.js.map' -o -name '*.json' ')' -exec file '{}' ';' | grep -F ': Mach-O' | cut -d: -f1)

failAtExit=''

colorRed=$(echo -e '\033[0;31m')
colorReset=$(echo -e '\033[0m')
errorPrefix="${colorRed}error${colorReset}"

while IFS= read -r file ; do
  #echo "Checking binary: ‘$file’…"

  # Check for shared libraries from ‘/nix/store’, excluding remaining reference
  # to the lib itself (`fileBasename` below – TODO: investigate why):
  fileBasename=$(basename "$file")
  otoolOutput=$(otool -L "$file" 2>&1)
  nixStoreRefs=$(echo "$otoolOutput" | grep -F '/nix/store/' | grep -vF "/${fileBasename} " || true)
  if [ -n "$nixStoreRefs" ] ; then
    echo "$errorPrefix: ‘$file’ references shared libraries inside ‘/nix/store’:"
    echo "$nixStoreRefs"
    echo
    failAtExit=1
  fi

  # Check if it’s signed:
  if [ "${file: -2}" != ".o" ] ; then # *.o files don’t need to be signed
    rc=0
    codesignOutput=$(codesign --display --verify --verbose=4 "$file" 2>&1) || rc=$?
    if [ $rc != 0 ] ; then
      if grep <<<"$codesignOutput" -qF 'code has no resources but signature indicates they must be present' ; then
        # let’s ignore this ↑ error – the signature *is* present
        :
      else
        echo "$errorPrefix: ‘$file’ appears unsigned:"
        echo "$codesignOutput" | sed -r 's/^/    /'
        echo
        failAtExit=1
      fi
    fi
  fi

done <<<"$allBinaries"

if [ -n "$failAtExit" ] ; then
  exit 1
fi
