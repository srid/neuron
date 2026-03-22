{
  description = "Future-proof note-taking and publishing based on Zettelkasten";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";

    nix-filter.url = "github:numtide/nix-filter/3c9e33ed627e009428197b07216613206f06ed80";
    nix-filter.flake = false;

    reflex-dom-pandoc = {
      url = "github:srid/reflex-dom-pandoc/b6a76c2c980a2bba9b4d170f95ef8b526ffea3a4";
      flake = false;
    };
    pandoc-link-context = {
      url = "github:srid/pandoc-link-context/85bd204339aafd309b8a3dd99ebffa6a50776cb6";
      flake = false;
    };
    directory-contents = {
      url = "github:srid/directory-contents/f8c7148121adcf5bae2f41b8265ce9cc4ed0556b";
      flake = false;
    };
    reflex-fsnotify = {
      url = "github:reflex-frp/reflex-fsnotify/cca674623b797dd423421dec0f1da952a1d1f36d";
      flake = false;
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, nix-filter, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      flake = {
        homeManagerModule = import ./home-manager-module.nix;
      };

      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      perSystem = { pkgs, system, ... }:
        let
          nix-filter-lib = import nix-filter;

          sources = {
            neuron = nix-filter-lib {
              root = ./.;
              name = "neuron";
              include = [
                "neuron-search"
                "neuron.cabal"
                (nix-filter-lib.inDirectory "exe")
                (nix-filter-lib.inDirectory "src")
                (nix-filter-lib.inDirectory "test")
              ];
            };
            reflex-dom-pandoc = inputs.reflex-dom-pandoc;
            pandoc-link-context = inputs.pandoc-link-context;
            directory-contents = inputs.directory-contents;
            reflex-fsnotify = inputs.reflex-fsnotify;
          };

          searchBuilder = ''
            mkdir -p $out/bin
            cp $src/neuron-search $out/bin/neuron-search
            chmod +x $out/bin/neuron-search
            wrapProgram $out/bin/neuron-search --prefix 'PATH' ':' ${
              pkgs.lib.makeBinPath [ pkgs.fzf pkgs.ripgrep pkgs.gawk pkgs.bat pkgs.findutils pkgs.envsubst ]
            }
            PATH=$PATH:$out/bin
          '';

          wrapSearchScript = drv: {
            buildTools = [ pkgs.makeWrapper ];
            preConfigure = searchBuilder;
          };

          inherit (pkgs.haskell.lib)
            overrideCabal doJailbreak dontCheck dontHaddock justStaticExecutables appendConfigureFlags;

          haskellOverrides = self: super: {
            pandoc-link-context = doJailbreak (self.callCabal2nix "pandoc-link-context" sources.pandoc-link-context { });
            reflex-dom-pandoc =
              dontHaddock (doJailbreak (overrideCabal (self.callCabal2nix "reflex-dom-pandoc" sources.reflex-dom-pandoc { }) (old: {
                postPatch = (old.postPatch or "") + ''
                  # GHC 9.10: forM no longer in scope, add explicit import
                  sed -i '/^import Control.Monad.Reader/a import Data.Traversable (forM)' src/Reflex/Dom/Pandoc/Footnotes.hs
                  # GHC 9.10: ~ requires TypeOperators
                  sed -i '1s/^/{-# LANGUAGE TypeOperators #-}\n/' src/Reflex/Dom/Pandoc/Raw.hs
                  # pandoc-types >= 1.23: Null constructor removed, replace with blank pattern
                  sed -i '/^  Null ->$/,/^    blank >> pure mempty$/d' src/Reflex/Dom/Pandoc/Document.hs
                '';
              })));
            fsnotify = dontCheck super.fsnotify;
            reflex-fsnotify =
              doJailbreak (self.callCabal2nix "reflex-fsnotify" sources.reflex-fsnotify { });


            # witherable 0.4.x (has Data.Witherable module that directory-contents needs)
            witherable = doJailbreak (self.callHackageDirect {
              pkg = "witherable";
              ver = "0.4.2";
              sha256 = "sha256-M5KOI2qKqf4qdfKUwQ2h+3pF5PfPhtz0dozPUAZVRL0=";
            } { });
            directory-contents = doJailbreak (self.callCabal2nix "directory-contents" sources.directory-contents { });

            neuron = appendConfigureFlags
              ((justStaticExecutables
                (doJailbreak (overrideCabal (self.callCabal2nix "neuron" sources.neuron { })
                  wrapSearchScript))).overrideDerivation (drv: {
                disallowedReferences = [
                  self.pandoc-types
                  self.warp
                  self.HTTP
                  self.js-jquery
                  self.js-dgtable
                  self.js-flot
                ];
                postInstall = ''
                  remove-references-to -t ${self.pandoc-types} $out/bin/neuron
                  remove-references-to -t ${self.warp} $out/bin/neuron
                  remove-references-to -t ${self.HTTP} $out/bin/neuron
                  remove-references-to -t ${self.js-jquery} $out/bin/neuron
                  remove-references-to -t ${self.js-dgtable} $out/bin/neuron
                  remove-references-to -t ${self.js-flot} $out/bin/neuron
                '';
              }))
              [ ];
          };

          haskellPackages = pkgs.haskellPackages.override {
            overrides = haskellOverrides;
          };

          nixShellSearchScript = pkgs.stdenv.mkDerivation {
            name = "neuron-search";
            src = sources.neuron;
            buildInputs = [ pkgs.makeWrapper ];
            buildCommand = searchBuilder;
          };
        in
        {
          formatter = pkgs.nixpkgs-fmt;

          packages = {
            default = haskellPackages.neuron;
            neuron = haskellPackages.neuron;
          };

          devShells.default = haskellPackages.shellFor {
            packages = p: [ p.neuron ];
            buildInputs = [
              pkgs.nixpkgs-fmt
              haskellPackages.ghcid
              haskellPackages.cabal-install
              haskellPackages.hlint
              haskellPackages.ormolu
              nixShellSearchScript
            ];
          };
        };
    };
}
