# This nix environmemnt is *just* for building the Gibbon compiler,
# nothing more.
#
# This is intended for use with stack/nix integration, i.e. stack's
# --nix argument.

# See ../.nix_default_environment.txt for the fixed version of nixpkgs
# which we test with in CI.  Using this file directly will use
# whatever nixpkgs your user account is currently pointing at.
with (import <nixpkgs> {});
# with import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-17.03.tar.gz) {};
# with import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/1e1472ed93e1256dfdac4908ae8edcb90ceff21c.tar.gz) {};

# Default should match the current LTS in stack.yaml:
{ghc ? haskell.compiler.ghc802 }:

haskell.lib.buildStackProject {
  inherit ghc;
  name = "basicGibbonEnv";
  buildInputs = [ gcc which ];
}
