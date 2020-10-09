# Installing

Neuron can be installed on Windows, Linux or macOS.

:::{.ui .inverted .large .blue .message}
If you want to give neuron a try before installing it, checkout [[cerveau]].
:::

## Prerequisites

### Nix

Neuron can be installed via the Nix[^nix] package manager. Install [Nix](https://nixos.org/) as follows:

``` bash
curl -L https://nixos.org/nix/install | sh
```

:::{.ui .floating .message}
:::{.header}
OS-specific notes
:::

* If you are on **Windows**, you should begin by [installing Ubuntu on WSL 2](https://docs.microsoft.com/en-us/windows/wsl/install-win10) (not WSL 1), before installing Nix on it.
  * Alternatively, you can simply download the static binary of the last stable release [here][staticbin].
* If you are on **macOS Catalina or later**, refer to the [macOS Installation](https://nixos.org/manual/nix/stable/#sect-macos-installation) section of the Nix manual on how to install Nix.
* If you are on **Linux**, and do not wish to install Nix, you may download the Linux static binary [here][staticbin]. Note that the static binary corresponds to the last stable release, which generally lags behind the development version (which the Nix install method uses).
:::

[staticbin]: https://public.srid.ca/list/neuron-1.0.1.0-linux.tar.gz
 
### Enable cache

Enable the [Nix cache](https://srid.cachix.org/) for neuron.

``` bash
# If you do not already have cachix, install it:
nix-env -iA cachix -f https://cachix.org/api/v1/install
# Use the cache to fetch binaries instead of compiling most packages.
cachix use srid
```

If you skip this step, your machine may compile several software for hours.

## Install neuron

To install the latest development version of neuron, run:

```bash
nix-env -if https://github.com/srid/neuron/archive/master.tar.gz
```

Note that this command can also *upgrade* your existing install of neuron.

For alternative mechanisms, see [[[2012401]]].

## Test your install

Make sure that you can execute the `neuron` executable. You should expect the following:

```bash
Usage: neuron [--version] [-d PATH] COMMAND
  Neuron, future-proof Zettelkasten app <https://neuron.zettel.page/>

Available options:
  --version                Show version
  -d PATH                  Run as if neuron was started in PATH instead of the
                           current working directory
  -h,--help                Show this help text

Available commands:
  new                      Create a new zettel
  open                     Open the locally generated Zettelkasten website
  search                   Search zettels and print the matching filepath
  query                    Run a query against the zettelkasten
  rib                      Generate static site via rib
```

## What's next?

Proceed to the [[tutorial]].

[^nix]: Nix is a general package manager that you can use to manage other software and services as well. [See here](https://github.com/srid/neuron/issues/193#issuecomment-629557917). If you do not wish to install Nix, try the [[[docker]]].
