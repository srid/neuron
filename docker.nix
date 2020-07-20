# Builds a docker image containing the neuron executable
#
# Run as:
#   docker load -i $(
#     nix-build docker.nix \
#       --argstr name <image-name> \
#       --argstr tag <image-tag>
#   )
let
  # TODO: Use the same nixpkgs used in project.nix (create a shared
  # nixpkgs.nix?)
  pkgs = import <nixpkgs> {};
  neuron = import ./. {};
in {
  name ? "sridca/neuron"
, tag ? "dev"
}: pkgs.dockerTools.buildImage {
  inherit name tag;
  contents = [ 
    neuron
    # These are required for the GitLab CI runner
    pkgs.coreutils 
    pkgs.bash_5 
  ];
}
