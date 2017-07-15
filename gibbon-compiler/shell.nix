# with (import <nixpkgs> {});
with import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-17.03.tar.gz) {};
# with import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/1e1472ed93e1256dfdac4908ae8edcb90ceff21c.tar.gz) {};

{ghc ? haskellPackages.ghc }:

haskell.lib.buildStackProject {
  inherit ghc;
  name = "basicGibbonEnv";
  buildInputs = [ gcc which ];
}
