args@{ system ? builtins.currentSystem, ...}:
let 
  pkgs = import ./dep/nixpkgs { inherit system; } ;
in (import ./project.nix { pkgs = pkgs; } ).neuron
