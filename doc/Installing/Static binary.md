---
slug: install-static
---

There is no [static binary](https://github.com/srid/neuron/issues/626) per se for current version of neuron, but a **self-contained Linux executable** is available [here](https://github.com/srid/neuron/releases/tag/1.9.35.0). As this was produced by the experimental [nix bundle](https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-bundle.html) feature, your mileage with it may vary.

Due to a [limitation](https://github.com/srid/neuron/issues/626#issuecomment-897575923) in nix-bundle you must *always* pass the absolute path to your notebook in the command line. For example, instead of running `neuron gen -wS`, run `neuron -d $(pwd) gen -wS`.

## v1.0

:::{.ui .warning .message}
**IMPORTANT**: These instructions for an *older version* of neuron. Static builds are currently unavailable[^why] for latest versions of neuron.
:::

[^why]: See <https://github.com/srid/neuron/pull/490#issuecomment-742085530>; specifically, we need someone willing to volunteer maintenance of static builds over time.

:::{.ui .warning .message}
**Note**: Some users have [reported problems](https://github.com/srid/neuron/issues/430#issuecomment-718597211) with the static binary; if you notice the same, just install using Nix. 
:::

Linux and Windows (WSL2) users can get the static binary [here][staticbin]. If you choose to use the static binary instead of installing through Nix (see [[Installing]]), note the following:

- You will have to *manually* install the runtime dependencies such as `fzf`, `bat`, `envsubst`, etc. yourself.
- The static binary corresponds to the last stable release, which generally lags behind the development version (which the Nix install method at [[Installing]] uses).

[staticbin]: https://github.com/srid/neuron/releases/download/1.0.1.0/neuron-1.0.1.0-linux.tar.gz
