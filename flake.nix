{
  description = "Future-proof note-taking and publishing based on Zettelkasten";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/4c87cb87a2db6b9eb43541c1cf83f2a2f725fa25";
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
