{
  pkgs ? import <nixpkgs> {}
, ...
}:

pkgs.writeShellScriptBin "neuron-search"
  ''
    set -euo pipefail
    NOTESDIR=''${1}
    cd ''${NOTESDIR}
    ${pkgs.ripgrep}/bin/rg --no-heading --no-line-number --sort path "title:" *.md \
      | ${pkgs.fzf}/bin/fzf --tac --no-sort -d ':' -n 3.. \
        --preview '${pkgs.bat}/bin/bat --style=plain --color=always {1}' \
        --bind 'ctrl-j:execute(xdg-open https://localhost:8080/(echo {1} | ${pkgs.gnused}/bin/sed "s/\.md/.html/g"))+abort' \
      | ${pkgs.gawk}/bin/awk -F: "{printf \"''${NOTESDIR}%s\", \$1}"
  ''
