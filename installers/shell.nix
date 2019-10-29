{ haskellCompiler ? "ghc865" }:
(import ./. { inherit haskellCompiler; }).shells.ghc
