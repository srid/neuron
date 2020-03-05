# neuron

Haskell meets Zettelkasten, for your plain-text delight.

**Design goals**

- A Haskell library for managing and rendering Zettelkasten notes
- Generate static site HTML
- Weblog adapter on top

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
bin/run
```

This launches a web server at http://localhost:8080 serving the statically generated content. Changing either the sources or the content in `./content` reloads everything.
