# neuron

**NOTE**: Not yet ready for public use.

neuron is a system for managing your plain-text [Zettelkasten](https://writingcooperative.com/zettelkasten-how-one-german-scholar-was-so-freakishly-productive-997e4e0ca125) notes. 

**Features**

- Static site generation of notes, for easy browsing
  - Graph-based automatic category tree view
- CLI for creating new zettel
- Exposed Haskell library to build your own system

See [srid.ca](https://github.com/srid/srid.ca) for a real-world example.

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

## Guide

`TODO` Write a nice guide (using neuron itself) to using neuron, including topics like:

- [ ] ZettelID (timestamp based, or custom text)
- [ ] Zettel graph and tree filtering
- [ ] Zettel link schemes: z://, zcf://
  - [ ] Zettel query: zquery://
- [ ] zettel CLI to make new zettels
