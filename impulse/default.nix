{ system ? builtins.currentSystem
}:
let 
  name = "impulse";
  p = import ./project.nix { inherit system; };
  pkgs = p.reflexPlatform.nixpkgs;
  app = pkgs.lib.getAttr name p.project.ghcjs;
in 
  pkgs.runCommand "${name}-site" {} ''
    mkdir -p $out
    # The original all.js is pretty huge; so let's run it by the closure
    # compiler.    
    # cp ${app}/bin/${name}.jsexe/all.js $out/
    ${pkgs.closurecompiler}/bin/closure-compiler \
        --externs=${app}/bin/${name}.jsexe/all.js.externs \
        --jscomp_off=checkVars \
        --js_output_file="$out/impulse.js" \
        -O ADVANCED \
        -W QUIET \
        ${app}/bin/${name}.jsexe/all.js
  ''
