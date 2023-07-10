{ inputs }:

let

  readClustersFile = fileName:
    let unique = builtins.foldl' (acc: e: if builtins.elem e acc then acc else acc ++ [ e ]) []; in
    unique (
      builtins.map builtins.unsafeDiscardStringContext (
        builtins.filter (el: builtins.isString el && el != "") (
          builtins.split "[ \n\r\t]+" (
            builtins.readFile fileName
          ))));

in rec {

  installerClusters = readClustersFile (inputs.self + "/installer-clusters.cfg");

  forEach = xs: fun: builtins.listToAttrs
    (builtins.map (cluster: { name = cluster; value = fun cluster; }) xs);

  forEachCluster = forEach installerClusters;

  # When did the project start? → `git show --no-patch --date=unix $(git rev-list --max-parents=0 HEAD)`
  daedalusEpoch = 1475675335;

  # In `std`, we don’t get `inputs.self.sourceInfo`. Instead, when Git
  # status is dirty, we get only `.rev = "not-a-commit"`. When clean,
  # we get all normal attributes. Cf.
  # <https://github.com/divnix/std/blob/d2bde49f82331db61ebabf7d0b7441a31364908a/src/grow.nix#L184>.

  buildRev =
    if inputs.self ? shortRev
    then inputs.self.rev
    else "0000000000000000000000000000000000000000";
  buildRevShort =
    if inputs.self ? shortRev
    then builtins.substring 0 9 buildRev
    else "dirty";
  # XXX: inputs.self.revCount is not available in Cicero, let’s use a counter incrementing every hour:
  buildCounter =
    if inputs.self ? shortRev
    then (inputs.self.lastModified - daedalusEpoch) / (60 * 60)
    else 0;

}
