# Builds a docker image containing the neuron executable
# Run as:
#   docker load -i $(nix-build docker.nix)
let
  pkgs = import <nixpkgs> {};
  neuron = import ./. {};
in {
  name ? "sridca/neuron"
, tag ? "test"
, includeShell ? false
}: pkgs.dockerTools.buildImage {
  name = name;
  tag = tag;
  contents = [ neuron ] ++ (if includeShell
    then [ pkgs.coreutils pkgs.bash_5 ]
    else []);
}
