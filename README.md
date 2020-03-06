# neuron

neuron is a system for managing your plain-text Zettelkasten notes. 

**Features**

- Static site generation of notes, for easy browsing
  - Graph-based automatic category tree view
- CLI for creating new zettel
- Exposed Haskell library to build your own system

See `./example`.

**Roadmap**

- Nix-based nvim for searching and editing Zettels
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

## Running example

To build and run the example:

```bash
bin/run-example
```
