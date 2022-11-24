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

  allClusters = installerClusters ++
    readClustersFile (inputs.self + "/installer-clusters-available.cfg");

  forEach = xs: fun: builtins.listToAttrs
    (builtins.map (cluster: { name = cluster; value = fun cluster; }) xs);

  forEachCluster = forEach allClusters;

  buildRev = if inputs.self ? revCount then inputs.self.rev else "0000000000000000000000000000000000000000";
  buildRevShort = if inputs.self ? revCount then builtins.substring 0 9 buildRev else "dirty";
  buildRevCount = inputs.self.revCount or 0;

}
