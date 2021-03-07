let
  nixpkgs = import ./dep/nixpkgs {};
in {
  pkgs ? nixpkgs,
  pkgsForBins ? null,
  neuronFlags ? [],
  disableHsLuaTests ? false,
  withHoogle ? false,
  ...
}:

let
  inherit (pkgs.haskell.lib)
    overrideCabal doJailbreak dontCheck justStaticExecutables appendConfigureFlags;
  inherit (import ./dep/gitignore { inherit (pkgs) lib; }) 
    gitignoreSource;
  inherit (import ./dep/nix-thunk {}) 
    thunkSource;
  # Deal with non-Nix Haskellers allowing broken symlinks in an otherwise
  # standalone Cabal package directory.
  fuckSymlinkAbuse = p: pkgs.haskell.lib.overrideCabal p (drv: {
    postUnpack = ''
      echo fuck > $sourceRoot/README.md
      echo fuck > $sourceRoot/LICENSE
    '';
  });
  unfuckCoLog = p: pkgs.haskell.lib.overrideCabal p (drv: {
    postUnpack = drv.postUnpack + ''
      # Ditch executable stanzas that fail to build
      # First 104 lines are enough to get the library.
      head -n 104 $sourceRoot/co-log.cabal > fuck
      mv fuck $sourceRoot/co-log.cabal
    '';
  });

  sources = {
    neuron = gitignoreSource ./.;
    reflex-dom-pandoc = thunkSource ./dep/reflex-dom-pandoc;
    pandoc-link-context = thunkSource ./dep/pandoc-link-context;
    directory-contents = thunkSource ./dep/directory-contents;
    reflex-fsnotify = thunkSource ./dep/reflex-fsnotify;
  };

  searchBuilder = ''
    mkdir -p $out/bin
    cp $src/neuron-search $out/bin/neuron-search
    chmod +x $out/bin/neuron-search
    wrapProgram $out/bin/neuron-search --prefix 'PATH' ':' ${
      with (if pkgsForBins != null then pkgsForBins else pkgs);
      lib.makeBinPath [ fzf ripgrep gawk bat findutils envsubst ]
    }
    PATH=$PATH:$out/bin
  '';
  wrapSearchScript = drv: {
    buildTools = [ pkgs.makeWrapper ];
    preConfigure = searchBuilder;
  };

  haskellOverrides = self: super: with pkgs.haskell.lib; {
    pandoc-link-context = self.callCabal2nix "pandoc-link-context" sources.pandoc-link-context {};
    reflex-dom-pandoc =
      dontHaddock (self.callCabal2nix "reflex-dom-pandoc" sources.reflex-dom-pandoc { });
    reflex-fsnotify = 
      # Jailbreak to allow newer base
      doJailbreak (self.callCabal2nix "reflex-fsnotify" sources.reflex-fsnotify {});

    # Test fails on pkgsMusl
    # https://github.com/hslua/hslua/issues/67
    hslua = if disableHsLuaTests then (dontCheck super.hslua) else super.hslua;

    directory-contents = self.callCabal2nix "directory-contents" sources.directory-contents {};

    neuron = appendConfigureFlags ((justStaticExecutables
      (overrideCabal (self.callCabal2nix "neuron" sources.neuron { })
        wrapSearchScript)).overrideDerivation (drv: {
          # Avoid transitive runtime dependency on the whole GHC distribution due to
          # Cabal's `Path_*` module thingy. For details, see:
          # https://github.com/NixOS/nixpkgs/blob/46405e7952c4b41ca0ba9c670fe9a84e8a5b3554/pkgs/development/tools/pandoc/default.nix#L13-L28
          #
          # In order to keep this list up to date, use nix-store and why-depends as
          # explained here: https://www.srid.ca/04b88e01.html
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
        })) neuronFlags;
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
