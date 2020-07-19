let 
  nixpkgsSrc = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/d7e20ee25ed8.tar.gz";
    sha256 = "1ar7prnrmmlqj17g57nqp82hgy5283dxb94akaqrwpbaz7qfwi4y";
  };
  gitignoreSrc = builtins.fetchTarball {
    url = "https://github.com/hercules-ci/gitignore/archive/c4662e6.tar.gz";
    sha256 = "1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
  };
in {
  system ? builtins.currentSystem,
  pkgs ? import nixpkgsSrc { inherit system; },
  # Cabal project name
  name ? "neuron",
  compiler ? pkgs.haskellPackages,
  withHoogle ? false,
  ...
}:

let
  inherit (pkgs.haskell.lib)
    overrideCabal markUnbroken doJailbreak appendPatch justStaticExecutables;

  inherit (import (gitignoreSrc) { inherit (pkgs) lib; }) gitignoreSource;

  sources = {
    neuron = gitignoreSource ./neuron;
    rib = import ./dep/rib/thunk.nix;
    commonmark = import ./dep/commonmark-hs/thunk.nix;
    reflex-dom-pandoc = import ./dep/reflex-dom-pandoc/thunk.nix;
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

  overrides = self: super: {
    # We include rib because it is quite tightly coupled with neuron development
    rib = self.callCabal2nix "rib" sources.rib { };

    # commonmark is not released on hackage yet
    commonmark =
      self.callCabal2nix "commonmark" (sources.commonmark + "/commonmark") { };
    commonmark-extensions = self.callCabal2nix "commonmark-extensions"
      (sources.commonmark + "/commonmark-extensions") { };
    commonmark-pandoc = self.callCabal2nix "commonmark-pandoc"
      (sources.commonmark + "/commonmark-pandoc") { };

    # Also not released yet
    reflex-dom-pandoc =
      self.callCabal2nix "reflex-dom-pandoc" sources.reflex-dom-pandoc { };

    # Override pandoc-types and dependencies because stack-lts versions are to old
    hslua = self.hslua_1_1_2;
    jira-wiki-markup = self.jira-wiki-markup_1_3_2;
    pandoc = self.pandoc_2_10;
    pandoc-types = self.pandoc-types_1_21;
    # pandoc-include-code sadly has not raised the version bound to the newer pandoc we picked
    pandoc-include-code = doJailbreak super.pandoc-include-code;

    # Version bumps for nixpkgs compat, have not been merged by upstream yet.
    dependent-sum-aeson-orphans = markUnbroken
      (appendPatch super.dependent-sum-aeson-orphans (pkgs.fetchpatch {
        url =
          "https://github.com/obsidiansystems/dependent-sum-aeson-orphans/commit/5a369e433ad7e3eef54c7c3725d34270f6aa48cc.patch";
        sha256 = "1lzrcicvdg77hd8j2fg37z19amp5yna5xmw1fc06zi0j95csll4r";
      }));

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

  localHaskellPackages = compiler.override { overrides = overrides; };

  nixShellSearchScript = pkgs.stdenv.mkDerivation {
    name = "neuron-search";
    src = sources.neuron;
    buildInputs = [ pkgs.makeWrapper ];
    buildCommand = searchBuilder;
  };

in {
  neuron = localHaskellPackages.neuron;
  shell = localHaskellPackages.shellFor {
    inherit withHoogle;
    packages = p: [ p.neuron ];
    buildInputs = [
      localHaskellPackages.ghcid
      localHaskellPackages.cabal-install
      nixShellSearchScript
    ];
  };
}
