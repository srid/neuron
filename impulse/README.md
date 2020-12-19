# impulse

WIP => https://github.com/srid/neuron/issues/108

## Prerequisites

Unless you enjoy compiling for hours at end, you should use the reflex-platform Nix cache by following the [instructions here][cache].

## Development

Running locally using GHC and jsaddle-warp:

```bash
nix-shell --run 'ghcid -T :main'
# Or, to run with a custom port
nix-shell --run 'JSADDLE_WARP_PORT=8080 ghcid -T :main'
```

Build JS using GHCJS:

```bash
# Assuming default.nix uses neuron from ../neuron
nix-build
cp ./result/impulse.js ../neuron/neuron/ghcjs/
# Then rebuild neuron
```

[cache]: https://github.com/obsidiansystems/obelisk#installing-obelisk
