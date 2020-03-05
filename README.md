# rib-sample

Sample site for the [Rib](https://github.com/srid/rib) static site generator

## Prerequisites

First, install the [Nix package manager](https://nixos.org/nix/):

``` bash
bash <(curl https://nixos.org/nix/install)
```

Optionally, enable the [Nix cache](https://srid.cachix.org/) if you would like to speed up local builds:

``` bash
# If you do not already have cachix, install it:
nix-env -iA cachix -f https://cachix.org/api/v1/install
# Enable nix cache for rib
cachix use srid
```

## Running

To build and run the site:

```bash
nix-shell --run 'ghcid -T :main --restart=src-dhall'
```

This launches a web server at http://localhost:8080 serving the statically
generated content. Changing either `Main.hs` or the content in `./content` reloads everything.

## Use a custom rib and port

You might have a local checkout of rib with certain modifications. And you might
want to run ghcid with the server running at a different port. Both of this can
achieved using the following command:

```bash
# Assuming rib is cloned at ../rib
nix-shell --arg rib ../rib --run 'ghcid -T ":main serve -p 9876" --restart=src-dhall'
```
