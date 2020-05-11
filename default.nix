let
  # To upgrade rib, go to https://github.com/srid/rib/commits/master, select the
  # revision you would like to upgrade to and set it here. Consult rib's
  # ChangeLog.md to check any notes on API migration.
  ribRevision = "7d345d8ee0cbe78009427935641afb074abd68b0 ";
  nixpkgsRev = "5f14d99efed3";
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
, pkgs ? import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz") {}
, compiler ? pkgs.haskell.packages.ghc865
, ...
}:

let 
  inherit (import (builtins.fetchTarball "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz") { inherit (pkgs) lib; })
    gitignoreSource;
    neuronSearchScript = pkgs.runCommand "neuron-search" { buildInputs = [ pkgs.makeWrapper ]; } ''
    mkdir -p $out/bin
    makeWrapper ${./.}/src-bash/neuron-search $out/bin/neuron-search --prefix 'PATH' ':' \
      "${pkgs.fzf}/bin:${pkgs.ripgrep}/bin:${pkgs.gawk}/bin:${pkgs.bat}/bin:${pkgs.findutils}/bin:${pkgs.envsubst}/bin"'';
  additional-packages = pkgs:
  [ neuronSearchScript
  ];
  excludeContent = path: typ: 
    let d = baseNameOf (toString path);
    in !(d == "guide" && typ == "directory");
  neuronSrc = gitignoreSource projectRoot;
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
    cat << EOF > $out/src/app/Neuron/Version/RepoVersion.hs
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE NoImplicitPrelude #-}
    module Neuron.Version.RepoVersion (version) where
    import Relude
    version :: Maybe Text
    version = Just "${neuronRev}"
    EOF
    '';
  sources = {
    dsum = builtins.fetchTarball "https://github.com/obsidiansystems/dependent-sum/archive/dependent-sum-0.6.2.2.tar.gz";
    # https://github.com/obsidiansystems/aeson-gadt-th/pull/22
    aeson-gadt-th = builtins.fetchTarball "https://github.com/srid/aeson-gadt-th/archive/ece1007.tar.gz";
    reflex-dom = builtins.fetchTarball "https://github.com/reflex-frp/reflex-dom/archive/94c26076a71f229b5bd27128cf6fc0dbcba011ac.tar.gz";

    # commonmark is not on Hackage
    commonmark = import ./dep/commonmark-hs/thunk.nix;
  };

in import rib { 
    inherit name compiler additional-packages; 
    root = if root == "" then neuronRoot else root;
    source-overrides = {
      neuron = neuronRoot;
      # Until https://github.com/obsidiansystems/which/pull/6 is merged
      which = builtins.fetchTarball "https://github.com/srid/which/archive/5061a97.tar.gz";
      dependent-sum = sources.dsum + "/dependent-sum";
      dependent-sum-template = sources.dsum + "/dependent-sum-template";
      aeson-gadt-th = sources.aeson-gadt-th;

      # commonmark
      commonmark = sources.commonmark + "/commonmark";
      commonmark-extensions = sources.commonmark + "/commonmark-extensions";
      commonmark-pandoc = sources.commonmark + "/commonmark-pandoc";
      emojis = import ./dep/emojis/thunk.nix;
      pandoc-types = import ./dep/pandoc-types/thunk.nix;
      pandoc = import ./dep/pandoc/thunk.nix;
      texmath = import ./dep/texmath/thunk.nix;
      hslua = import ./dep/hslua/thunk.nix;
      doctemplates = import ./dep/doctemplates/thunk.nix;
      doclayout = import ./dep/doclayout/thunk.nix;
      jira-wiki-markup = import ./dep/jira-wiki-markup/thunk.nix;

      # reflex-platform: matching its reflex overlay
      prim-uniq = builtins.fetchTarball "https://github.com/obsidiansystems/prim-uniq/archive/0.1.0.1.tar.gz";
      patch = builtins.fetchTarball "https://github.com/reflex-frp/patch/archive/25f202b4c05fe7f319a606667bb6873e1d386f56.tar.gz";
      reflex-dom-core = sources.reflex-dom + "/reflex-dom-core";
      chrome-test-utils = sources.reflex-dom + "/chrome-test-utils";
      reflex = builtins.fetchTarball "https://github.com/reflex-frp/reflex/archive/3e81151f7d9a8b0e5d735ae2e0e438b3acc64e7b.tar.gz";
    } // source-overrides;

    overrides = self: super: with pkgs.haskell.lib; {
      base-noprelude = null;

      dependent-map = self.callHackage "dependent-map" "0.3.1.0" {};
      witherable = self.callHackage "witherable" "0.3.1" {};
      some = self.callHackage "some" "1.0.0.3" {};
      reflex = dontCheck super.reflex;
      reflex-dom-core = dontCheck super.reflex-dom-core;
      patch = dontCheck super.patch;

      # We must add neuron-search as a runtime dependency to the 'neuron'
      # Haskell package so that other apps `import`ing this defafult.nix would
      # know where to find when building the neuron library dependency through
      # cabal (instead of directly via nix).
      neuron = super.neuron.overrideDerivation (drv: {
        propagatedBuildInputs = drv.propagatedBuildInputs ++ [neuronSearchScript];
      });
    };
  }
