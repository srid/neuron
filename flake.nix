{
  description = "Future-proof note-taking and publishing based on Zettelkasten";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/10bb036486138341e4c760c4c98ddadef56fff7b";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        project = import ./project.nix { inherit pkgs; };

      in
      rec {
        packages = { neuron = project.neuron; };
        defaultPackage = packages.neuron;

        apps = { neuron = flake-utils.lib.mkApp { drv = packages.neuron; }; };
        defaultApp = apps.neuron;

        devShell = project.shell;
      });
}
