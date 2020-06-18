let 
  ghc = (import ./project.nix {}).ghc;
  neuron = ghc.neuron;
in 
  neuron.overrideDerivation (drv: {
     disallowedReferences = [ ghc.pandoc ghc.pandoc-types ghc.shake ghc.warp ghc.HTTP ];
     postInstall = ''
      remove-references-to -t ${ghc.pandoc} $out/bin/neuron
      remove-references-to -t ${ghc.pandoc-types} $out/bin/neuron
      remove-references-to -t ${ghc.shake} $out/bin/neuron
      remove-references-to -t ${ghc.warp} $out/bin/neuron
      remove-references-to -t ${ghc.HTTP} $out/bin/neuron
     '';
  })
