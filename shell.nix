let
  sources = import nix/sources.nix;
  pkgs = import sources.nixpkgs {};

in pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
    cabal-install
    ghcid
    haskell-language-server
    tmate
  ];
}
