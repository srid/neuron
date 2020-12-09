args@{...}:
let 
  pkgs = import ./dep/nixpkgs {} ;
in (import ./project.nix { pkgs = pkgs; } ).neuron
