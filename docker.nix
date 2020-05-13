# Builds a docker image containing the neuron executable
# Run as:
#   docker load -i $(nix-build docker.nix)
let
  pkgs = import <nixpkgs> {};
  baseImage = pkgs.dockerTools.pullImage {
    imageName = "lnl7/nix";
    finalImageTag = "2.3.3";
    imageDigest = "sha256:a969b4f238eab9298fe426fc90cc45efe01685431a419ca2907f10ddbbed2b7f";
    sha256 = "1zy4zwlka5c4hgjbgy8xh1ikyb3z2lwzfjwv8y0cyzx1danys4vk";
  };
in

pkgs.dockerTools.buildImage {
  fromImage = baseImage;
  name = "sridca/neuron";
  tag = "test";
  contents = [ (import ./.) ];
}
