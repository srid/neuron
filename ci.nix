let
  project = import ./project.nix {};
  pkgs = project.nixpkgs;
in
  # Build both default.nix and shell.nix so that both derivations are pushed
  # to cachix. This allows the development workflow (bin/run, etc.) to use
  # cachix to full extent.
  { 
    neuron = project.ghc.neuron;
    neuronShell = project.shells.ghc;
  }
    
