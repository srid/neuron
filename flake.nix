{
  description = "Future-proof note-taking and publishing based on Zettelkasten";

  nixConfig = {
    extra-substituters = [ "https://srid.cachix.org" ];
    extra-trusted-public-keys = [ "srid.cachix.org-1:MTQ6hBfSLVJ7E0MXBYNf1MHhAPJWIjno6K7ECCnFMVc=" ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

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

  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, nix-filter, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ haskell-flake.flakeModule ];

      flake = {
        homeManagerModule = import ./home-manager-module.nix;
      };

      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      perSystem = { self', pkgs, lib, ... }:
        let
          nix-filter-lib = import nix-filter;

          neuronSource = nix-filter-lib {
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

          searchBuilder = ''
            mkdir -p $out/bin
            cp $src/neuron-search $out/bin/neuron-search
            chmod +x $out/bin/neuron-search
            wrapProgram $out/bin/neuron-search --prefix 'PATH' ':' ${
              pkgs.lib.makeBinPath [ pkgs.fzf pkgs.ripgrep pkgs.gawk pkgs.bat pkgs.findutils pkgs.envsubst ]
            }
            PATH=$PATH:$out/bin
          '';

          inherit (pkgs.haskell.lib)
            overrideCabal doJailbreak dontCheck dontHaddock justStaticExecutables;
        in
        {
          haskellProjects.default = {
            projectRoot = neuronSource;
            autoWire = [ "packages" "checks" "devShells" ];

            packages = {
              reflex-dom-pandoc.source = inputs.reflex-dom-pandoc;
              pandoc-link-context.source = inputs.pandoc-link-context;
              directory-contents.source = inputs.directory-contents;
              reflex-fsnotify.source = inputs.reflex-fsnotify;
            };

            settings = {
              reflex-dom-pandoc.jailbreak = true;
              reflex-dom-pandoc.haddock = false;
              reflex-fsnotify.jailbreak = true;
              directory-contents.jailbreak = true;
              pandoc-link-context.jailbreak = true;
              neuron.jailbreak = true;
              neuron.buildFromSdist = false;
            };

            otherOverlays = [
              (hself: hsuper: {
                # Patch reflex-dom-pandoc for GHC 9.10 / pandoc-types 1.23
                reflex-dom-pandoc = overrideCabal hsuper.reflex-dom-pandoc (old: {
                  postPatch = (old.postPatch or "") + ''
                    sed -i '/^import Control.Monad.Reader/a import Data.Traversable (forM)' src/Reflex/Dom/Pandoc/Footnotes.hs
                    sed -i '1s/^/{-# LANGUAGE TypeOperators #-}\n/' src/Reflex/Dom/Pandoc/Raw.hs
                    sed -i '/^  Null ->$/,/^    blank >> pure mempty$/d' src/Reflex/Dom/Pandoc/Document.hs
                  '';
                });
                fsnotify = dontCheck hsuper.fsnotify;
                # witherable 0.4.x (has Data.Witherable module)
                witherable = doJailbreak (hself.callHackageDirect {
                  pkg = "witherable";
                  ver = "0.4.2";
                  sha256 = "sha256-M5KOI2qKqf4qdfKUwQ2h+3pF5PfPhtz0dozPUAZVRL0=";
                } { });
                # neuron custom: wrap search script + strip references
                neuron =
                  let
                    wrapped = overrideCabal hsuper.neuron (old: {
                      buildTools = (old.buildTools or [ ]) ++ [ pkgs.makeWrapper ];
                      preConfigure = searchBuilder;
                    });
                  in
                  (justStaticExecutables wrapped).overrideDerivation (drv: {
                    disallowedReferences = [
                      hself.pandoc-types
                      hself.warp
                      hself.HTTP
                      hself.js-jquery
                      hself.js-dgtable
                      hself.js-flot
                    ];
                    postInstall = ''
                      remove-references-to -t ${hself.pandoc-types} $out/bin/neuron
                      remove-references-to -t ${hself.warp} $out/bin/neuron
                      remove-references-to -t ${hself.HTTP} $out/bin/neuron
                      remove-references-to -t ${hself.js-jquery} $out/bin/neuron
                      remove-references-to -t ${hself.js-dgtable} $out/bin/neuron
                      remove-references-to -t ${hself.js-flot} $out/bin/neuron
                    '';
                  });
              })
            ];

            devShell = {
              tools = hp: {
                ghcid = hp.ghcid;
                cabal-install = hp.cabal-install;
                hlint = hp.hlint;
                ormolu = hp.ormolu;
              };
              mkShellArgs.packages = [ pkgs.nixpkgs-fmt ];
            };
          };

          formatter = pkgs.nixpkgs-fmt;
          packages.default = self'.packages.neuron;
          packages.dockerImage = pkgs.dockerTools.buildImage {
            name = "sridca/neuron";
            tag = "dev";
            copyToRoot = pkgs.buildEnv {
              name = "neuron-docker-root";
              paths = [
                self'.packages.neuron
                pkgs.coreutils
                pkgs.bashInteractive
              ];
              pathsToLink = [ "/bin" ];
            };
            config = {
              Env = [
                "LANG=en_US.UTF-8"
                "LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive"
              ];
              WorkingDir = "/notes";
              Volumes = {
                "/notes" = { };
              };
            };
          };
        };
    };
}
