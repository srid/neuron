let
  gitignoreSrc = builtins.fetchTarball {
    url = "https://github.com/hercules-ci/gitignore/archive/c4662e6.tar.gz";
    sha256 = "1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
  };
in {
  system ? builtins.currentSystem,
  pkgs ? import (import ./nixpkgs.nix) { inherit system; },
  # Cabal project name
  name ? "neuron",
  compiler ? pkgs.haskellPackages,
  withHoogle ? false,
  ...
}:

let
  inherit (pkgs.haskell.lib)
    overrideCabal doJailbreak dontCheck justStaticExecutables;

  inherit (import (gitignoreSrc) { inherit (pkgs) lib; }) gitignoreSource;

  thunkOrPath = dep:
    let p = ./dep + "/${dep}/thunk.nix";
    in if builtins.pathExists p then import p else (./dep + "/${dep}");

  sources = {
    neuron = gitignoreSource ./neuron;
    rib = thunkOrPath "rib";
    reflex-dom-pandoc = thunkOrPath "reflex-dom-pandoc";
  };

  searchBuilder = ''
    mkdir -p $out/bin
    cp $src/src-bash/neuron-search $out/bin/neuron-search
    chmod +x $out/bin/neuron-search
    wrapProgram $out/bin/neuron-search --prefix 'PATH' ':' ${
      with pkgs;
      lib.makeBinPath [ fzf ripgrep gawk bat findutils envsubst ]
    }
    PATH=$PATH:$out/bin
  '';
  wrapSearchScript = drv: {
    buildTools = [ pkgs.makeWrapper ];
    preConfigure = searchBuilder;
  };

  haskellOverrides = self: super: {
    rib-core = self.callCabal2nix "rib-core" (sources.rib + "/rib-core") { };

    reflex-dom-pandoc =
      pkgs.haskell.lib.dontHaddock (self.callCabal2nix "reflex-dom-pandoc" sources.reflex-dom-pandoc { });

    # This version is not the default in nixpkgs, yet.
    skylighting = super.skylighting_0_10_0_2;
    skylighting-core = super.skylighting-core_0_10_0_2;
    # Jailbreak pandoc to work with newer skylighting
    pandoc = doJailbreak (dontCheck super.pandoc);

    neuron = (justStaticExecutables
      (overrideCabal (self.callCabal2nix "neuron" sources.neuron { })
        wrapSearchScript)).overrideDerivation (drv: {
          # Avoid transitive runtime dependency on the whole GHC distribution due to
          # Cabal's `Path_*` module thingy. For details, see:
          # https://github.com/NixOS/nixpkgs/blob/46405e7952c4b41ca0ba9c670fe9a84e8a5b3554/pkgs/development/tools/pandoc/default.nix#L13-L28
          #
          # In order to keep this list up to date, use nix-store and why-depends as
          # explained here: https://www.srid.ca/04b88e01.html
          disallowedReferences = [
            self.pandoc
            self.pandoc-types
            self.shake
            self.warp
            self.HTTP
            self.js-jquery
            self.js-dgtable
            self.js-flot
          ];
          postInstall = ''
            remove-references-to -t ${self.pandoc} $out/bin/neuron
            remove-references-to -t ${self.pandoc-types} $out/bin/neuron
            remove-references-to -t ${self.shake} $out/bin/neuron
            remove-references-to -t ${self.warp} $out/bin/neuron
            remove-references-to -t ${self.HTTP} $out/bin/neuron
            remove-references-to -t ${self.js-jquery} $out/bin/neuron
            remove-references-to -t ${self.js-dgtable} $out/bin/neuron
            remove-references-to -t ${self.js-flot} $out/bin/neuron
          '';
        });
  };

  haskellPackages = compiler.override { overrides = haskellOverrides; };

  nixShellSearchScript = pkgs.stdenv.mkDerivation {
    name = "neuron-search";
    src = sources.neuron;
    buildInputs = [ pkgs.makeWrapper ];
    buildCommand = searchBuilder;
  };

in {
  neuron = haskellPackages.neuron;
  shell = haskellPackages.shellFor {
    inherit withHoogle;
    packages = p: [ p.neuron ];
    buildInputs = [
      haskellPackages.ghcid
      haskellPackages.cabal-install
      haskellPackages.haskell-language-server
      haskellPackages.hlint
      haskellPackages.ormolu
      nixShellSearchScript
    ];
  };
}
