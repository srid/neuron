---
slug: install
---

Neuron can be installed on Windows, Linux or macOS.

## Prerequisites

### Nix

Neuron can be installed via the Nix[^nix] package manager. Install [Nix](https://nixos.org/) as follows:

```shell
curl -L https://nixos.org/nix/install | sh
```

:::{.ui .floating .message}
:::{.header}
OS-specific notes
:::

* If you are on **Windows**, you should begin by [installing Ubuntu on WSL 2](https://docs.microsoft.com/en-us/windows/wsl/install-win10) (not WSL 1), before installing Nix on it.
* If you are on **macOS Catalina or later**, refer to the [macOS Installation](https://nixos.org/manual/nix/stable/#sect-macos-installation) section of the Nix manual on how to install Nix.

As an alternative to Nix, you may try the [[Docker workflow]]#.
:::

[staticbin]: https://github.com/srid/neuron/releases/download/1.0.1.0/neuron-1.0.1.0-linux.tar.gz
 
### Enable cache (optional)

Enable the [Nix cache](https://srid.cachix.org/) for neuron.

```shell
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use srid
```

If you skip this step, your machine will spend some time compiling the neuron source code.

## Install neuron

To install the latest development version (see [[Neuron v1]] if you are looking for an older version) of neuron, run:

```shell
nix-env -if https://github.com/srid/neuron/archive/master.tar.gz
```

Note that this command can also *upgrade* your existing install of neuron.

For alternative mechanisms, see [[Declarative Install]]#.

## Test your install

Make sure that you can execute the `neuron` executable. You should expect the following:

```
Usage: neuron [--version] [-d PATH] [-o PATH] COMMAND
  Neuron, future-proof Zettelkasten app <https://neuron.zettel.page/>

Available options:
  --version                Show version
  -d PATH                  Run as if neuron was started in PATH instead of the
                           current working directory
  -o PATH                  Custom path to generate static site in
  -h,--help                Show this help text

Available commands:
  gen                      Generate and serve the static site
  new                      Create a new zettel
  open                     Open the local static site
  search                   Search zettels and print their path
  query                    Query the zettelkasten in JSON
```

## What's next?

Proceed to the [[Tutorial]].

[^nix]: Nix is a general package manager that you can use to manage other software and services as well. [See here](https://github.com/srid/neuron/issues/193#issuecomment-629557917).
