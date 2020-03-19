# neuron

neuron is a system for managing your plain-text [Zettelkasten](https://writingcooperative.com/zettelkasten-how-one-german-scholar-was-so-freakishly-productive-997e4e0ca125) notes. 

**Features**

- Static site generation of notes, for easy browsing
  - Graph-based automatic category tree view
- CLI for creating new zettel
- Use the `neuron` executable to work with a directory of your notes, or extend in Haskell using the neuron library.

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

To run neuron, you will need a directory that holds all your zettel files. This repo comes up with sample zettelkasten at `./guide`, and you may create your own based on that one.

- Run `nix-build` to build the "neuron" executable
- Run `./result/bin/neuron ./guide rib serve`

This should generate HTML for the guide zettelkasten in this repo, and spin up a web server at http://localhost:8080 where you can view it.

### Creating new zettels

```
vim $(./result/bin/neuron ./guide new "Some title")
```

## Documentation

See [neuron.srid.ca](https://neuron.srid.ca/) for a full guide to neuron.
