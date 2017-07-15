
# This Nix environment contains everything needed to run the tests.

# Default GHC should match the current LTS in stack.yaml:
{ pkgs ? import <nixpkgs> {}
, ghc ? pkgs.haskell.compiler.ghc7103 }:

with pkgs;

haskell.lib.buildStackProject {
  inherit ghc;
  name = "basicGibbonEnv";
  buildInputs = [ stdenv ghc stack which ncurses racket ];
}
