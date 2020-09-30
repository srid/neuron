args@{...}:
let 
  sources = import nix/sources.nix {};
  pkgs = import sources.nixpkgs args;
in (import ./project.nix { pkgs = pkgs; } ).neuron
