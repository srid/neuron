<img width="10%" src="./assets/logo.svg">

# neuron

[![BSD3](https://img.shields.io/badge/License-BSD-blue.svg)](https://en.wikipedia.org/wiki/BSD_License)
[![built with nix](https://img.shields.io/badge/Built_With-Nix-5277C3.svg?logo=nixos&labelColor=73C3D5)](https://builtwithnix.org)
[![Zulip chat](https://img.shields.io/badge/zulip-join_chat-brightgreen.svg)](https://funprog.zulipchat.com/#narrow/stream/231929-Neuron)

neuron is a system for managing your plain-text [Zettelkasten](https://neuron.zettel.page/2011401.html) notes. 

**Features**

- Extended Markdown for easy linking between zettels
- Web interface (auto generated static site)
- Graph view of zettels (organic category tree)
- CLI for creating new zettels with automatic ID

## Getting started

See [neuron.zettel.page](https://neuron.zettel.page/) for the full guide to installing and using neuron.

## Developing

When modifying `src/Neuron`, use ghcid as instructed as follows to monitor compile errors:

```bash
nix-shell --run ghcid
```

You can test your changes by running it on the `./guide` (or any) zettelkasten as follows:

```bash
bin/run -d ./guide rib -wS
```

This command will also automatically recompile and restart when you change any of the Haskell source files.

### Running tests

Unit tests can be run via ghcid as follows:

```
bin/test
```
