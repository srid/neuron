{ system ? builtins.currentSystem }:
let
  sources = import ./nix/sources.nix {};
  pkgs = import sources.nixpkgs { inherit system; };
in
pkgs.recurseIntoAttrs {
  # Build both default.nix and shell.nix such that both derivations are
  # pushed to cachix. This allows the development workflow (bin/run, etc.) to
  # use cachix to full extent.
  neuron = import ./default.nix {};
  neuronShell = import ./shell.nix {};
}
