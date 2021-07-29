args@{ ... }:
let
  nixpkgs = import ./nixpkgs.nix { };
  nixpkgsStatic = import ./nixpkgs.nix {
    overlays = [
      (self: super: {
        # https://github.com/NixOS/nixpkgs/issues/131557
        python3 = super.python3.override { enableLTO = false; };
        haskellPackages = super.haskellPackages.override {
          overrides = hself: hsuper: with pkgs.haskell.lib; {
            # https://github.com/hslua/hslua/issues/67
            # hslua = dontCheck super.hslua;
            # Tests are flaky
            time-compat = dontCheck hsuper.time-compat;
            # neuron = dontCheck hsuper.neuron;
          };
        };
      })
    ];
  };
  pkgs = nixpkgsStatic.pkgsMusl;
in
(import ./project.nix {
  inherit pkgs;
  # We have to use original nixpkgs for fzf, etc. otherwise this will give
  #   error: missing bootstrap url for platform x86_64-unknown-linux-musl
  pkgsForBins = nixpkgs;
  neuronFlags = [
    "--ghc-option=-optl=-static"
    # Disabling shared as workaround. But - https://github.com/nh2/static-haskell-nix/issues/99#issuecomment-665400600
    # TODO: Patch ghc bootstrap binary to use ncurses6, which might also obviate the nixpkgs revert.
    "--disable-shared"
    "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
    "--extra-lib-dirs=${pkgs.zlib.static}/lib"
    "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
    "--extra-lib-dirs=${pkgs.ncurses.override { enableStatic = true; }}/lib"
  ];
}).neuron

