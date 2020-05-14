builtins.mapAttrs (system: _v:
  let
    project = import ./project.nix { inherit system; };
    url = "https://github.com/NixOS/nixpkgs/archive/2255f292063ccbe184ff8f9b35ce475c04d5ae69.tar.gz";
    pkgs = import (builtins.fetchTarball url) { inherit system; };
  in
  pkgs.recurseIntoAttrs {
  
    # Build both default.nix and shell.nix so that both derivations are pushed
    # to cachix. This allows the development workflow (bin/run, etc.) to use
    # cachix to full extent.
    neuron = project.ghc.neuron;
    neuronShell = project.shells.ghc;
  }
) {
  x86_64-linux = {};
  x86_64-darwin = {};
}

