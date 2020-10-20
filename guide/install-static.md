# Static binary 

Linux and Windows (WSL2) users can get the static binary [here][staticbin]. If you choose to use the static binary instead of installing through Nix (see [[install]]), note the following:

- You will have to *manually* install the runtime dependencies such as `fzf`, `bat`, `envsubst`, etc. yourself.
- The static binary corresponds to the last stable release, which generally lags behind the development version (which the Nix install method at [[install]] uses).

[staticbin]: https://github.com/srid/neuron/releases/download/1.0.1.0/neuron-1.0.1.0-linux.tar.gz
