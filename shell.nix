{ ghcVersion ? "ghc92", pkgs ? import <nixpkgs> { } }:

let
  haskellPackages = pkgs.haskell.packages.${ghcVersion};

  ghc = haskellPackages.ghcWithHoogle (h:
    with h; [
      containers
      filepath
      megaparsec
      mtl
      parser-combinators
      process
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
