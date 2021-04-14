{
  description = "Future-proof note-taking and publishing based on Zettelkasten";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/fcab19deb78fbb5ea24e19b133cf34358176396a";
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
