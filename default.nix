let
  # To upgrade rib, go to https://github.com/srid/rib/commits/master, select the
  # revision you would like to upgrade to and set it here. Consult rib's
  # ChangeLog.md to check any notes on API migration.
  ribRevision = "40c8f8d06e09637f7858c0d069954ff00e10c6a2";

  inherit (import (builtins.fetchTarball "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz") { }) gitignoreSource;
  pkgs = import <nixpkgs> {};
  excludeContent = path: typ: 
    let d = baseNameOf (toString path);
    in !(d == "guide" && typ == "directory");
  neuronRoot = pkgs.lib.cleanSourceWith { filter = excludeContent; src = gitignoreSource ./.; };
in {
# Rib library source to use
  rib ? builtins.fetchTarball "https://github.com/srid/rib/archive/${ribRevision}.tar.gz"
# Cabal project root
, root ? neuronRoot
# Cabal project name
, name ? "neuron"
, source-overrides ? {}
, ...
}:

let 
  neuronSearchScript = pkgs.writeShellScriptBin "neuron-search"
    ''
      set -euo pipefail
      ${pkgs.ripgrep}/bin/rg --no-heading --no-line-number --sort path "title:" *.md \
        | ${pkgs.fzf}/bin/fzf --tac --no-sort -d ':' -n 3.. \
          --preview '${pkgs.bat}/bin/bat --style=plain --color=always {1}' \
          --bind 'ctrl-j:execute(xdg-open https://localhost:8080/(echo {1} | ${pkgs.gnused}/bin/sed "s/\.md/.html/g"))+abort' \
        | ${pkgs.gawk}/bin/awk -F: '{printf "%s", $1}'
    '';
  additional-packages = pkgs: with pkgs; 
  [ neuronSearchScript
  ];
  neuron = import rib { 
    inherit root name additional-packages; 
    source-overrides = {
      neuron = neuronRoot;
      # Until https://github.com/obsidiansystems/which/pull/6 is merged
      which = builtins.fetchTarball "https://github.com/srid/which/archive/5061a97a4e03ba2c0971f52c8af503fdf56ef9ba.tar.gz";
    } // source-overrides;
  };
in if pkgs.lib.inNixShell 
  # Defer to rib's use of `developPackage` if in nix-shell
  then neuron  
  # Wrap the final derivation with its runtime dependencies, so that
  # staticWhich will work on the user's machine.
  else pkgs.stdenv.mkDerivation {
    name = "neuron";
    propagatedBuildInputs = additional-packages pkgs;
    unpackPhase = "true";
    installPhase = ''
      mkdir -p $out/bin
      ln -s ${neuron}/bin/neuron $out/bin/neuron
      '';
  }

