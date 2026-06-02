{ pkgs, ... }:
{
  packages = with pkgs; [
    haskellPackages.fourmolu
    haskellPackages.hspec-golden
  ];
  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc912;
  };
  languages.javascript = {
    enable = true;
    npm.enable = true;
  };
}
