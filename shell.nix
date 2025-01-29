{ parallel ? false }:
let
  pkgs = import (builtins.fetchGit {
                   url = "https://github.com/nixos/nixpkgs/";
                   ref = "refs/tags/24.05";
                 }) {};

  clang = pkgs.clang_16;
  llvm = pkgs.llvm_16;
  gibbon_dir = builtins.toString ./.;

  # Conditionally include old gcc stdenv
  std = if parallel then pkgs.gcc7Stdenv else pkgs.gcc13Stdenv;
in
  with pkgs;
  let commonPackages = [
    # C/C++
    clang llvm uthash
    # Rust
    rustc cargo
    # Racket
    racket
    # Other utilities
    ncurses unzip which rustfmt clippy ghcid
  ];
  # Only include gcc7 if parallel is set
  gcc = if parallel then [gcc7] else [gcc13];
  # Exclude gdb, rr, and valgrind on ARM Mac
  extra = if pkgs.hostPlatform.config != "aarch64-apple-darwin"
          then [gdb rr valgrind] else [];
  in mkShell.override { stdenv = std; } ({
    # we use default Haskell toolchain supplied with the chosen nixpkgs; this way we hit their cache
    inputsFrom = [ (pkgs.haskellPackages.callCabal2nix "gibbon-compiler" ./gibbon-compiler { }).env ];

    name = "basicGibbonEnv";
    buildInputs = commonPackages ++ gcc ++ extra;

    shellHook = ''
      export GIBBONDIR=${gibbon_dir}
    '';
  })
