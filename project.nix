{ system ? builtins.currentSystem }:
(import ./dep/reflex-platform { inherit system; }).project ({ pkgs, hackGet, ... }: 
let 
  commonmark = hackGet ./dep/commonmark-hs;
  skylighting = hackGet ./dep/skylighting;
  test-framework = hackGet ./dep/test-framework;

  neuronSearchScript = pkgs.runCommand "neuron-search" { buildInputs = [ pkgs.makeWrapper ]; } 
    ''
    mkdir -p $out/bin
    makeWrapper ${./.}/src-bash/neuron-search $out/bin/neuron-search --prefix 'PATH' ':' \
        "${pkgs.fzf}/bin:${pkgs.ripgrep}/bin:${pkgs.gawk}/bin:${pkgs.bat}/bin:${pkgs.findutils}/bin:${pkgs.envsubst}/bin"
    '';

in {
  shellToolOverrides = ghc: super: {
    inherit neuronSearchScript;
  };

  packages = {
    neuron = ./.;
    # TODO: expose these overrides so it can be used in the other project
    #that uses neuron as a thunk.

    reflex-dom-pandoc = hackGet ./dep/reflex-dom-pandoc;
    shake = hackGet ./dep/shake;
    
    # commonmark
    commonmark = commonmark + "/commonmark";
    commonmark-extensions = commonmark + "/commonmark-extensions";
    commonmark-pandoc = commonmark + "/commonmark-pandoc";
    emojis = hackGet ./dep/emojis;
    pandoc-types = hackGet ./dep/pandoc-types;
    texmath = hackGet ./dep/texmath;
    hslua = hackGet ./dep/hslua;
    doctemplates = hackGet ./dep/doctemplates;
    doclayout = hackGet ./dep/doclayout;
    jira-wiki-markup = hackGet ./dep/jira-wiki-markup;

    test-framework = test-framework + "/core";
    # test-framework-th = null;
    test-framework-hunit = test-framework + "/hunit";
    test-framework-quickcheck2 = test-framework + "/quickcheck2";

    # pandoc
    pandoc = hackGet ./dep/pandoc;
    #skylighting = skylighting + "/skylighting";
    #skylighting-core = skylighting + "/skylighting-core";
    regex-base = hackGet ./dep/regex-base;
    regex-posix = hackGet ./dep/regex-posix;
    regex-pcre = hackGet ./dep/regex-pcre;
    regex-pcre-builtin = hackGet ./dep/regex-pcre-builtin;

    algebraic-graphs = hackGet ./dep/alga;
    clay = hackGet ./dep/clay;
    HsYAML = hackGet ./dep/HsYAML;

    rib = hackGet ./dep/rib;
  };

  overrides = self: super: with pkgs.haskell.lib; let 
    skylighting-core = overrideCabal super.skylighting-core (drv: {
      isExecutable = true;
      isLibrary = true;
      configureFlags = [ "-fexecutable" ];  # We need the CLI tool later.
    });

    # Strip off the library part, so we trim out dependencies to only those
    # needed by the executable.
    #
    # FIXME: This causes cyclic references, with the binary depending on the
    # library derivation (`strings` on the executable reports
    # "lib/ghc-8.6.5/x86_64-linux-ghc-8.6.5" etc).
    #
    # Might also be related to this:
    #  https://github.com/NixOS/cabal2nix/issues/433
    makeExecutable = x: overrideCabal x (drv: {
      enableSeparateBinOutput = true;
      enableSeparateDataOutput = true;
    });
  in {
    neuron = super.neuron.overrideDerivation (drv: {
        propagatedBuildInputs = drv.propagatedBuildInputs ++ [neuronSearchScript];
    });

    shake = dontCheck super.shake;
    modern-uri = dontCheck (self.callHackageDirect {
        pkg = "modern-uri";
        ver = "0.3.2.0";
        sha256 = "14pr856vwva3gjv573rxfs2mg30icwkk4avyi682cnr0ld8yq8iw"; 
    } {});
    megaparsec = dontCheck (self.callHackageDirect {
        pkg = "megaparsec";
        ver = "8.0.0";
        sha256 = "1bk4jsa69maryj97jcvxxc211icvnkr21xrj2bqq9ddfizkq5lg0"; 
    } {});
    mmark = dontCheck (self.callHackageDirect {
        pkg = "mmark";
        ver = "0.0.7.2";
        sha256 = "04024dbrnxq8hrv553knahn90wlrxrggqamgx8axcnrhhxxvknz2"; 
    } {});
    mmark-ext = doJailbreak (dontCheck (self.callHackageDirect {
        pkg = "mmark-ext";
        ver = "0.2.1.2";
        sha256 = "05mcxhnclblzaqq4blmpk4mp37rq7b19jfq6vwxz15629gfxby7p"; 
    } {}));

    relude = self.callHackageDirect {
        pkg = "relude";
        ver = "0.6.0.0";
        sha256 = "1x2d7w3dm10lcz5k9ryy1hy8mwh53cjnriqzz7rqfxxcxnsgzl5l";
    } {};

    with-utf8 = self.callHackageDirect {
        pkg = "with-utf8";
        ver = "1.0.1.0";
        sha256 = "129bsyawcmfig1m3cch91d4nn6wlji3g5lm26jkf08yp54l76lrq"; 
    } {};
    connection = self.callHackageDirect {
        pkg = "connection";
        ver = "0.3.1";
        sha256 = "0qjdz2fxxszbns7cszhnkwm8x8l3xlnad6iydx2snfi416sypiy0"; 
    } {};
    haddock-library = dontCheck (self.callHackageDirect {
        pkg = "haddock-library";
        ver = "1.9.0";
        sha256 = "12nr4qzas6fzn5p4ka27m5gs2rym0bgbfrym34yp0cd6rw9zdcl3"; 
    } {});
    optparse-applicative = self.callHackageDirect {
        pkg = "optparse-applicative";
        ver = "0.15.1.0";
        sha256 = "1mii408cscjvids2xqdcy2p18dvanb0qc0q1bi7234r23wz60ajk"; 
    } {};
    base-compat = self.callHackageDirect {
        pkg = "base-compat";
        ver = "0.10.5";
        sha256 = "0fq38x47dlwz3j6bdrlfslscz83ccwsjrmqq6l7m005331yn7qc6"; 
    } {};
    dhall = dontCheck (self.callHackageDirect {
        pkg = "dhall";
        ver = "1.30.0";
        sha256 = "1iqvn3kalb5q8j1czx7r6qxfrsng5jrxzyx5gzzxcw46bpjig8n5"; 
    } {});
    prettyprinter = dontCheck (self.callHackageDirect {
        pkg = "prettyprinter";
        ver = "1.6.1";
        sha256 = "05hccfk3bvdlginx95skyfh9cwr3126zf9qiz0bmmzpms98q37p9"; 
    } {});
    atomic-write = dontCheck (self.callHackageDirect {
        pkg = "atomic-write";
        ver = "0.2.0.7";
        sha256 = "1r9ckwljdbw3mi8rmzmsnh89z8nhw2qnds9n271gkjgavb6hxxf3"; 
    } {});
    cborg-json = dontCheck (self.callHackageDirect {
        pkg = "cborg-json";
        ver = "0.2.2.0";
        sha256 = "1s7pv3jz8s1qb0ydcc5nra9f63jp4ay4d0vncv919bakf8snj4vw"; 
    } {});

    algebraic-graphs = dontCheck super.algebraic-graphs;
    clay = dontCheck super.clay;
    Glob = dontCheck super.Glob;
    exception-transformers = dontCheck super.exception-transformers;
    exceptions = dontCheck super.exceptions;
    SHA = dontCheck super.SHA;
    aeson = dontCheck (self.callHackage "aeson" "1.4.5.0" {});
    aeson-diff = dontCheck super.aeson-diff;
    blaze-builder = dontCheck super.blaze-builder;
    base64-bytestring = dontCheck super.base64-bytestring;
    network-uri = dontCheck super.network-uri;
    hashable = dontCheck super.hashable;
    http-types = dontCheck super.http-types;
    lucid = dontCheck super.lucid;
    cereal = dontCheck super.cereal;
    pandoc = dontHaddock (dontCheck super.pandoc);

    quickcheck-instances = null;
    serialise = dontCheck super.serialise;
    vector-builder = dontCheck super.vector-builder;
    edit-distance-vector = dontCheck super.edit-distance-vector;
    hslua = dontCheck super.hslua;
    parsers = dontCheck super.parsers;

    # Try to use the latest version for fixes.
    skylighting-core = dontCheck (self.callHackageDirect {
        pkg = "skylighting-core";
        ver = "0.8.4";
        sha256 = "10k7r67a14pxlwd4zd0j06s3gdh09g6bjgsn6mlg3aha9nj52rgl"; 
    } {});
    skylighting = dontCheck (self.callHackageDirect {
        pkg = "skylighting";
        ver = "0.8.4";
        sha256 = "0vcw9p0hs0s21xrxymbpbjzz6f8x7683y25p5kl4d9m9x6kblb9c"; 
    } {});
  };

  shells = {
    ghc = ["neuron"];
  };
})