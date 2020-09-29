args@{...}:
let 
  sources = import nix/sources.nix {};
  pkgs = import sources.nixpkgs-static args;
in (import ./project.nix { pkgs = pkgs.pkgsMusl; disableHsLuaTests = true; } ).neuron
