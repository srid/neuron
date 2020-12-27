{ system ? builtins.currentSystem
, withHoogle ? false
}:
let 
  inherit (import ../dep/gitignore { }) gitignoreSource;
  reflexPlatform = import ./dep/reflex-platform { 
    inherit system;
  };
  project = reflexPlatform.project ({pkgs, hackGet ,...}: 
    let 
      neuronSrc = pkgs.lib.cleanSource (gitignoreSource ../neuron);
    in {
      inherit withHoogle;
      useWarp = true;

      packages = let 
        cm = hackGet ./dep/commonmark;
      in {
        relude = hackGet ./dep/relude;
        # TODO: Ignore bin, .vscode, etc.
        impulse = pkgs.lib.cleanSource (gitignoreSource ./.);

        # neuron & its dependencies (not already in reflex-platform)
        # TODO: Use hackGet for direct/no-reload development?
        neuron = pkgs.runCommand "neuron" { buildInputs = [ ]; }
          ''
          mkdir -p $out/src
          cp -r -p ${neuronSrc}/{ghcjs,src-bash,test} $out/
          cp -r -p ${neuronSrc}/src/lib $out/src/
          # Remove non-library stanzas (expected to be at the end)
          LINE=`grep -n LIBMARKER ${neuronSrc}/neuron.cabal | cut -f1 -d:`
          head -n $LINE ${neuronSrc}/neuron.cabal > $out/neuron.cabal
          '';
        directory-contents = import ../dep/directory-contents/thunk.nix;
        reflex-dom-pandoc = import ../dep/reflex-dom-pandoc/thunk.nix;
        pandoc-link-context = import ../dep/pandoc-link-context/thunk.nix;
        commonmark = cm + "/commonmark";
        commonmark-pandoc = cm + "/commonmark-pandoc";
        commonmark-extensions = cm + "/commonmark-extensions";
        algebraic-graphs = hackGet ./dep/alga;
        clay = hackGet ./dep/clay;
        # neuron requires >= 0.2.5.0
        aeson-gadt-th = hackGet ./dep/aeson-gadt-th;
      };

      overrides = self: super: with pkgs.haskell.lib; {
        neuron = if withHoogle then super.neuron else dontHaddock super.neuron;
        algebraic-graphs = dontCheck super.algebraic-graphs;  # Test fails
      };

      shells = {
        ghc = ["impulse"];
        ghcjs = ["impulse"];
      };

      shellToolOverrides = 
        let nixpkgs = import ../dep/nixpkgs { };
        in _: _: with nixpkgs.haskell.packages.ghc865; {
          inherit haskell-language-server;
        };
    });
in {
  inherit project reflexPlatform;
}
