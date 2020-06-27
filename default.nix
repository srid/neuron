let 
  ghc = (import ./project.nix {}).ghc;
  neuron = ghc.neuron;
in 
  neuron.overrideDerivation (drv: {
    # Avoid transitive runtime dependency on the whole GHC distribution due to
    # Cabal's `Path_*` module thingy. For details, see:
    # https://github.com/NixOS/nixpkgs/blob/46405e7952c4b41ca0ba9c670fe9a84e8a5b3554/pkgs/development/tools/pandoc/default.nix#L13-L28
    #
    # In order to keep this list up to date, use nix-store and why-depends as
    # explained here: https://www.srid.ca/04b88e01.html
    disallowedReferences = [ 
      ghc.pandoc 
      ghc.pandoc-types 
      ghc.shake ghc.warp 
      ghc.HTTP 
      ghc.js-jquery 
      ghc.js-dgtable 
      ghc.js-flot 
    ];
    postInstall = ''
      remove-references-to -t ${ghc.pandoc} $out/bin/neuron
      remove-references-to -t ${ghc.pandoc-types} $out/bin/neuron
      remove-references-to -t ${ghc.shake} $out/bin/neuron
      remove-references-to -t ${ghc.warp} $out/bin/neuron
      remove-references-to -t ${ghc.HTTP} $out/bin/neuron
      remove-references-to -t ${ghc.js-jquery} $out/bin/neuron
      remove-references-to -t ${ghc.js-dgtable} $out/bin/neuron
      remove-references-to -t ${ghc.js-flot} $out/bin/neuron
    '';
  })
