{
  pkgs ? import <nixpkgs> {}
, ...
}:

pkgs.writeShellScriptBin "neuron-search"
  ''
    set -euo pipefail
    NOTESDIR=''${1}
    FILTERBY=''${2}
    SEARCHFROMFIELD=''${3}
    OPENCMD=`${pkgs.envsubst}/bin/envsubst -no-unset -no-empty <<< ''${4}`
    cd ''${NOTESDIR}
    ${pkgs.ripgrep}/bin/rg --no-heading --no-line-number --sort path "''${FILTERBY}" *.md \
      | ${pkgs.fzf}/bin/fzf --tac --no-sort -d ':' -n ''${SEARCHFROMFIELD}.. \
        --preview '${pkgs.bat}/bin/bat --style=plain --color=always {1}' \
        --bind 'ctrl-j:execute(xdg-open https://localhost:8080/(echo {1} | ${pkgs.gnused}/bin/sed "s/\.md/.html/g"))+abort' \
      | ${pkgs.gawk}/bin/awk -F: "{printf \"''${NOTESDIR}/%s\", \$1}" \
      | ${pkgs.findutils}/bin/xargs -r ''${OPENCMD}
  ''
