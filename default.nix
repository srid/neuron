let
  # To upgrade rib, go to https://github.com/srid/rib/commits/master, select the
  # revision you would like to upgrade to and set it here. Consult rib's
  # ChangeLog.md to check any notes on API migration.
  ribRevision = "5562878dbe1b0c0983c4b2e4e2310a51d743bd51";

  inherit (import (builtins.fetchTarball "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz") { }) gitignoreSource;
  pkgs = import <nixpkgs> {};
  excludeContent = path: typ: 
    let d = baseNameOf (toString path);
    in !(d == "guide" && typ == "directory");
  neuronRoot = pkgs.lib.cleanSourceWith { filter = excludeContent; src = gitignoreSource ./.; };
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
      with-utf8 = builtins.fetchTarball "https://github.com/serokell/haskell-with-utf8/archive/v1.0.0.0.tar.gz";
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
