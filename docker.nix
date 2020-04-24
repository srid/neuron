# Builds a docker image with latest neuron from master branch.
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
  neuron =
    let src = builtins.fetchGit { url = "https://github.com/srid/neuron"; ref = "master"; };
    in import src.outPath { gitRev = src.shortRev; };
in

pkgs.dockerTools.buildImage {
  fromImage = baseImage;
  name = "sridca/neuron";
  tag = "test";
  contents = [ neuron ];
}
