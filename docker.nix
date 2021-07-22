# Builds a docker image containing the neuron executable
#
# Run as:
#   docker load -i $(
#     nix-build docker.nix \
#       --argstr name <image-name> \
#       --argstr tag <image-tag>
#   )
let
  pkgs = import ./nixpkgs.nix { };
  neuron = (import ./project.nix { }).neuron;
in
{ name ? "sridca/neuron"
, tag ? "dev"
}: pkgs.dockerTools.buildImage {
  inherit name tag;
  contents = [
    neuron
    # These are required for the GitLab CI runner
    pkgs.coreutils
    pkgs.bash_5
  ];

  config = {
    Env = [
      # For i18n to work (with filenames, etc.)
      "LANG=en_US.UTF-8"
      "LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive"
    ];
    WorkingDir = "/notes";
    Volumes = {
      "/notes" = { };
    };
  };
}
