# Builds a docker image containing the neuron executable
# Run as:
#   docker load -i $(
#     nix-build docker.nix \
#       --arg name '"<image name>"' \
#       --arg tag '"<image tag>"'
#   )
let
  pkgs = import <nixpkgs> {};
  neuron = import ./. {};
in {
  name ? "sridca/neuron"
, tag ? "test"
}: pkgs.dockerTools.buildImage {
  name = name;
  tag = tag;
  contents = [ 
    neuron
    # These are required for the GitLab CI runner
    pkgs.coreutils 
    pkgs.bash_5 
  ];
}
