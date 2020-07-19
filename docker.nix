# Builds a docker image containing the neuron executable
# Run as:
#   docker load -i $(nix-build docker.nix)
let
  pkgs = import <nixpkgs> {};
  neuron = import ./. {};
in

pkgs.dockerTools.buildImage {
  name = "sridca/neuron";
  tag = "test";
  contents = [ neuron ];
}
