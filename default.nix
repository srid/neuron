let
  # To upgrade rib, go to https://github.com/srid/rib/commits/master, select the
  # revision you would like to upgrade to and set it here. Consult rib's
  # ChangeLog.md to check any notes on API migration.
  ribRevision = "2dcd420";
  projectRoot = ./.;
in {
# Rib library source to use
  rib ? builtins.fetchTarball "https://github.com/srid/rib/archive/${ribRevision}.tar.gz"
# Cabal project root
, root ? ""
# Cabal project name
, name ? "neuron"
, gitRev ? ""
, source-overrides ? {}
, pkgs ? import <nixpkgs> {}
, ...
}:

let 
  inherit (import (builtins.fetchTarball "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz") { }) gitignoreSource;
  neuronSearchScript = pkgs.callPackage ./src-script/neuron-search { inherit pkgs; };
  additional-packages = pkgs:
  [ neuronSearchScript
    # For PureScript dev
    pkgs.purescript
    pkgs.spago
  ];
  excludeContent = path: typ: 
    let d = baseNameOf (toString path);
    in !(d == "guide" && typ == "directory");
  neuronSrc = pkgs.lib.cleanSourceWith { filter = excludeContent; src = gitignoreSource projectRoot; };
  gitDescribe = pkgs.runCommand "neuron-gitDescribe" 
    { buildInputs = [ pkgs.git ]; }
    ''
      mkdir $out
      git -C ${projectRoot} describe --long --always --dirty | tr -d '\n' > $out/output
    '';
  neuronRev = if gitRev == "" then builtins.readFile (gitDescribe + /output) else gitRev;
  # Overwrite src/Neuron/Version.hs as git won't be available in the Nix derivation.
  neuronRoot = pkgs.runCommand "neuron" { buildInputs = [ neuronSrc ]; }
    ''
    mkdir $out
    cp -r -p ${neuronSrc}/* $out/
    chmod -R u+w $out/
    cat << EOF > $out/src/Neuron/Version/RepoVersion.hs
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE NoImplicitPrelude #-}
    module Neuron.Version.RepoVersion (version) where
    import Relude
    version :: Text
    version = "${neuronRev}"
    EOF
    '';

in import rib { 
    inherit name additional-packages; 
    root = if root == "" then neuronRoot else root;
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
