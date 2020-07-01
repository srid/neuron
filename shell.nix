{ system ? builtins.currentSystem, withHoogle ? false }:
(import ./project.nix { inherit system withHoogle; }).shells.ghc
