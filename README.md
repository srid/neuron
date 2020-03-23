<img width="10%" src="./assets/logo.svg">

# neuron

neuron is a system for managing your plain-text [Zettelkasten](https://neuron.srid.ca/2011401.html) notes. 

**Features**

- Extended Markdown for easy linking between zettels
- Web interface (auto generated static site)
- Graph view of zettels (organic category tree)
- CLI for creating new zettels with automatic ID

## Getting started

See [neuron.srid.ca](https://neuron.srid.ca/) for the full guide to installing and using neuron.

## Developing

When modifying `src/Neuron`, use ghcid as instructed as follows to monitor compile errors:

```bash
nix-shell --run ghcid
```

You can test your changes by running it on the `./guide` zettelkasten as follows:

```bash
nix-shell --run 'ghcid -c "cabal new-repl exe:neuron" -T ":main ./guide rib serve"'
```
