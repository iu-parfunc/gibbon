
# This Nix environment contains everything needed to run the tests.

# Default GHC should match the current LTS in stack.yaml:
{ pkgs ? import (fetchTarball (import ./.nix_default_environment.txt)) {}
, ghc ? pkgs.haskell.compiler.ghc802 }:

with pkgs;

haskell.lib.buildStackProject {
  inherit ghc;
  name = "basicGibbonEnv";
  buildInputs = [ stdenv ghc stack which ncurses racket ];
}
