{ ghcVersion ? "ghc92", pkgs ? import <nixpkgs> { } }:

let
  haskellPackages = pkgs.haskell.packages.${ghcVersion};

  ghc = haskellPackages.ghcWithHoogle (h:
    with h; [
      containers
      filepath
      lens
      megaparsec
      mtl
      parser-combinators
      partial-order
      process
      recursion-schemes
      text
      utf8-string
    ]);

in pkgs.mkShell {
  buildInputs = with pkgs; [
    cabal-install
    ghc
    haskellPackages.haskell-language-server
    hlint
    stylish-haskell
    tinycc
  ];
}
