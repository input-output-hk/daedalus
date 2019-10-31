{ haskellCompiler ? "ghc865" }:
(import ./. { inherit haskellCompiler; }).shell
