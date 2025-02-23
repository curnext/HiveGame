{ pkgs ? import <nixos> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.ghc
  ];
}
