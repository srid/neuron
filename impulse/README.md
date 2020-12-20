# impulse

The impulse feature of neuron is implemented in this sub-project. It is built using GHCJS, and thus relies on reflex-platform. However, the development workflow uses only GHC.

## Prerequisites

Unless you enjoy compiling for hours at end, you should use the reflex-platform Nix cache by following the [instructions here][cache].

## Development

Running locally using GHC and jsaddle-warp:

```bash
ln -s /path/to/your/neuron/output/dir/cache.json cache.json
nix-shell --run 'ghcid -T :main'
# Or, to run with a custom port
nix-shell --run 'JSADDLE_WARP_PORT=8080 ghcid -T :main'
```

Run `nix-build` to build the full JS. See neuron's bin/build-impulse.js which does this.

[cache]: https://github.com/obsidiansystems/obelisk#installing-obelisk
