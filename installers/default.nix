{ system ? builtins.currentSystem
, config ? {}
, pkgs ? localLib.iohkNix.getPkgs { inherit system config; }

# Disable running of tests for all local packages.
, forceDontCheck ? false

# Enable profiling for all haskell packages.
# Profiling slows down performance by 50% so we don't enable it by default.
, enableProfiling ? false

# Enable separation of build/check derivations.
, enableSplitCheck ? false

# Keeps the debug information for all haskell packages.
, enableDebugging ? false

# Build (but don't run) benchmarks for all local packages.
, enableBenchmarks ? false

# Overrides all nix derivations to add build timing information in
# their build output.
, enablePhaseMetrics ? true

# Overrides all nix derivations to add haddock hydra output.
, enableHaddockHydra ? false

# Disables optimization in the build for all local packages.
, fasterBuild ? false
, daedalus-bridge
, localLib
}:

with pkgs;
with haskell.lib;

let
  inherit daedalus-bridge;
  addTestStubsOverlay = import ./overlays/add-test-stubs.nix {
    inherit pkgs daedalus-bridge;
  };
  # We use GHC 8.2.2 because too many changes are needed to get to build with 8.4.3
  haskellPackages = callPackage localLib.iohkNix.haskellPackages {
    inherit forceDontCheck enableProfiling enablePhaseMetrics enableHaddockHydra
      enableBenchmarks fasterBuild enableDebugging enableSplitCheck;
    pkgsGenerated = haskell.packages.ghc822;
    ghc = haskell.compiler.ghc822;
    filter = localLib.isDaedalus;
    requiredOverlay = ./overlays/required.nix;
    customOverlays = [ addTestStubsOverlay ];
  };


in
  justStaticExecutables (haskellPackages.daedalus-installer)
