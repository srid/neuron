let
  # To upgrade rib, go to https://github.com/srid/rib/commits/master, select the
  # revision you would like to upgrade to and set it here. Consult rib's
  # ChangeLog.md to check any notes on API migration.
  ribRevision = "2c437baa9387606432cbbcdaec8ec2776096cd50";

  inherit (import (builtins.fetchTarball "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz") { }) gitignoreSource;
  pkgs = import <nixpkgs> {};
  excludeContent = path: typ: 
    let d = baseNameOf (toString path);
    in !(d == "guide" && typ == "directory");
  projectRoot = ./.;
  neuronSrc = pkgs.lib.cleanSourceWith { filter = excludeContent; src = gitignoreSource projectRoot; };
  gitDescribe = pkgs.runCommand "neuron-gitDescribe" 
    { buildInputs = [ pkgs.git ]; }
    ''
      mkdir $out
      git -C ${projectRoot} describe --long --always > $out/output
    '';
  neuronRoot = pkgs.runCommand "neuron" { buildInputs = [ pkgs.git gitDescribe ]; }
    ''
    mkdir $out
    cp -r -p ${neuronSrc}/* $out/
    chmod -R u+w $out/
    GITDESC=`cat ${gitDescribe}/output`
    cat << EOF > $out/src/Neuron/Zettelkasten/Version.hs
    module Neuron.Zettelkasten.Version where
    version :: String
    version = "$GITDESC"
    EOF
    '';
in {
# Rib library source to use
  rib ? builtins.fetchTarball "https://github.com/srid/rib/archive/${ribRevision}.tar.gz"
# Cabal project root
, root ? neuronRoot
# Cabal project name
, name ? "neuron"
, source-overrides ? {}
, ...
}:

let 
  neuronSearchScript = pkgs.callPackage ./src-script/neuron-search { inherit pkgs; };
  additional-packages = pkgs:
  [ neuronSearchScript
  ];
in import rib { 
    inherit root name additional-packages; 
    source-overrides = {
      neuron = neuronRoot;
      # Until https://github.com/obsidiansystems/which/pull/6 is merged
      which = builtins.fetchTarball "https://github.com/srid/which/archive/5061a97.tar.gz";
    } // source-overrides;
    overrides = self: super: with pkgs.haskell.lib; {
      # We must add neuron-search as a runtime dependency to the 'neuron'
      # Haskell package so that other apps `import`ing this defafult.nix would
      # know where to find when building the neuron library dependency through
      # cabal (instead of directly via nix).
      neuron = super.neuron.overrideDerivation (drv: {
        propagatedBuildInputs = drv.propagatedBuildInputs ++ [neuronSearchScript];
      });
    };
  }
