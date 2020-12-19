{ withHoogle ? false, system ? builtins.currentSystem,...}:

(import ./project.nix {inherit withHoogle system; }).project.shells.ghc
