#!/bin/sh

set -e

CLUSTERS="$(cat $(dirname $0)/../../installer-clusters.cfg | xargs echo -n)"
OSES="linux macos64 win64"

cd $(dirname $0)
for os in ${OSES}
do for cluster in ${CLUSTERS}
   do echo "checking ${cluster}-${os}"
      dhall --explain <<< "./launcher.dhall (./${cluster}.dhall ./${os}.dhall) ./${os}.dhall" > /dev/null
      dhall --explain <<< "./topology.dhall (./${cluster}.dhall ./${os}.dhall)"               > /dev/null
      done; done
