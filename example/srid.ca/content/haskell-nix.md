---
title: "Nix recipes for Haskellers"
description: How to develop Haskell projects using Nix
category: 'Programming'
---

The goal of this article is to get you comfortable managing simple [Haskell](https://www.haskell.org/) programs and projects using the **Nix** package manager without going too much into the details.

```toc
```

## Prerequisites

You are running either Linux or macOS, and have installed the **Nix** package manager using [these instructions](https://nixos.org/nix/) [1](footnote:1). You do *not* need to install anything else, including needing to install Haskell, as Nix will manage that for you.

## Simple programs

Let us begin with the simplest Haskell program, try to compile and run it with the help of Nix.

```haskell
-- HelloWorld.hs
module Main where

main :: IO ()
main = putStrLn "Hello World"
```

Haskell code is compiled by GHC, which is provided by the Nix package called "ghc". How do we install it? According to the [Nix manual](https://nixos.org/nix/manual/#chap-package-management) this can be done by running the command `nix-env -i ghc`. For Haskell developers, there is a better approach. Instead of installing packages in a global environment, you may install them to an *isolated* area and launch a shell with those packages in its environment. This is done using the `nix-shell -p ghc` command.

```bash
# This drops us in a bash shell with ghc package installed and 
# $PATH updated.
$ nix-shell -p ghc
...
# Now let's run our module.
[nix-shell:~] runhaskell HelloWorld.hs
Hello World
```

As you can see, nix-shell dropped us in a shell environment with the "ghc" package installed and activated. This puts `runhaskell` (part of the "ghc" package) in your PATH, running which will compile and run your first Haskell program. When you exit the nix-shell ([Ctrl+D][kbd]), `runhaskell` will no longer be in scope, however the "ghc" package will have been cached so that subsequent invocations of `nix-shell -p ghc` would not have to download and install it once again. [2](footnote:2)

### Using library dependencies

What if our program relied on an third-party Haskell library? The following program uses the [brick](http://hackage.haskell.org/package/brick) UI library.

```haskell
-- HelloWorldUI.hs
module Main where

import Brick

ui :: Widget ()
ui = str "Hello, world!"

main :: IO ()
main = simpleMain ui
```

We can no longer use the "ghc" package here. Fortunately, Nix is also a programming language, and as such as we can evaluate arbitrary Nix expressions to create a customized environment. Official Nix packages come from the [nixpkgs](https://github.com/NixOS/nixpkgs) channel, which provides a function called `ghcWithPackages`. Evaluating this function, passing it a list of Haskell libraries (already in nixpkgs), will create an environment with both GHC and the specific Haskell libraries installed. 

```bash
$ nix-shell \
    -p "haskellPackages.ghcWithPackages (p: [p.brick])" \
    --run "runhaskell HelloWorld.hs"
```

The `--run` argument will invoke the given command instead of dropping us in an interactive shell. This single command does *so much*---install the Haskell compiler with the requested libraries, compile our program and run it!

### Haskell scripts

You can use the above nix-shell command in the shebang to create self-contained Haskell scripts. Let us see an example, but using [ghcid](https://github.com/ndmitchell/ghcid), instead of runhaskell:

```haskell
-- myscript.hs
#! /usr/bin/env -S"ANSWER=42" nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.shower])"
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

import Shower (printer)
import System.Environment (getEnv)

main :: IO ()
main = do
  let question = "The answer to life the universe and everything"
  answer <- getEnv "ANSWER"
  printer (question, "is", answer)
```

Run `chmod u+x myscript.hs` to make it an executable, and then run it as `./myscript.hs`. Not only is it a self-sufficient script (depending on nothing but nix in the environment), but thanks to ghcid it also re-compiles and re-launches itself whenever it changes! See more examples [here](https://github.com/srid/aoc2019).

## Cabal project

Haskell projects normally use [cabal](https://www.haskell.org/cabal/), and you might already be familiar with [Stack](https://haskellstack.org/) which uses Cabal underneath. Nix is an alternative to Stack with many advantanges, chief of them being the creation of reproducible development environments using declarative configuration that handles even non-Haskell packages.

Adding Nix support to most Cabal projects is a matter of creating a file called `default.nix` in the project root (just make sure you have a .cabal file named appropriately). This file is by default used by commands like `nix-build` and `nix-shell`, which we will use when developing the project.

```nix
# default.nix
let 
  pkgs = import <nixpkgs> { };
in 
  pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          ghcid
        ]);
  }
```

Now if you run `nix-shell` it will drop you in a shell with all Haskell dependencies (from .cabal file) installed. This will be your development shell; from here you can run your usual `cabal` commands, and everything will function as expected.

```bash
$ nix-shell
...
[nix-shell:~] cabal new-build
..
```

If you only want to *build* the project, creating a final executable, use `nix-build`.

### Development dependencies

Notice the `modifier` attribute in the previous example. It specifies a list of build dependencies, using the `addBuildTools` function, that becomes available when we run either `nix-shell` or `nix-build`. Here, you will specify all the packages you need for development.[3](footnote:3) We speficied two---`cabal` and `ghcid`. If you removed `cabal` from this list, then cabal will not be in scope of your nix-shell. We added [`ghcid`](https://github.com/ndmitchell/ghcid), which can be used to run a daemon that will recompile your project if any of the source files change; go ahead and give it a try using `nix-shell --run ghcid`.

### Overriding dependencies

The above will work as long as the libraries your project depends on exist on nixpkgs (which itself is derived from Stackage). That will not always be the case and you may want to *override* certain dependencies.

In Nix overriding library packages is rather straightforward. The aforementioned `developPackage` function exposes this capability via the `source-overrides` attribute. Suppose your cabal project depends on the [named](https://github.com/monadfix/named) package at a particular git revision (`e684a00`), then you would modify your `default.nix` to look like:

```nix
let
  pkgs = import <nixpkgs> { };
  compilerVersion = "ghc865"; 
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
  compiler.developPackage {
    root = ./.;
    source-overrides = {
      named = builtins.fetchTarball 
        "https://github.com/monadfix/named/archive/e684a00.tar.gz";
    };
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          ghcid
        ]);
  }
```

Now, if you re-run `nix-shell` or `nix-build` Nix will rebuild your package, and any packages depending on `named`, using the new source.

Note that this example also demonstrates how to select a compiler version.

### Multi-package cabal project

`developPackage` cannot be used if you use the cabal project feature, containing multiple packages. You will have to go a few steps lower in the abstraction ladder, and use the underlying Nix functions (`callCabal2nix`, `shellFor`, `extend`, `inNixShell`) in the `default.nix` of a multiple-package cabal project. See [summoner's default.nix](https://github.com/kowainik/summoner/blob/master/default.nix) for a full example.

## Caching

Nix has builtin support for caching. Packages from the nixpkgs channel are already cached in the official cache. If you want to provide caching for your own packages, you may use [nix-serve](https://nixos.wiki/wiki/Binary_Cache) (from NixOS) or [Cachix](https://cachix.org/) (third-party service).

## Continuous Integration

Setting up CI for a Haskell project that already uses Nix is rather simple. If you use Github and Cachix, the easiest way is to use [the cachix Github Action](https://github.com/cachix/cachix-action). [4](footnote:4) 

## External links

* [Nix Pills](https://nixos.org/nixos/nix-pills/): a tutorial series on using Nix
* [Haskell section of Nix manual](https://nixos.org/nixpkgs/manual/#haskell) ([github link](https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/haskell.section.md))
* [haskell.nix](https://github.com/input-output-hk/haskell.nix): some people recommend haskell.nix for complicated setups

---

> footnotes
 
  1. Alternatively, if you are feeling adventurous enough, you may install [NixOS](https://nixos.org/), a Linux distribution based on Nix.
  2. Nix is a *general* package manager. You may use it to manage not only haskell packages, but also any other program. For example, to temporarily use the `tree` package, so as to dispay the directory tree of the current directory, you would run: `nix-shell -p tree --run tree`.
  3. Use `pkgs.lib.haskell.inNixShell` to conditionally include dependencies on
     nix-shell but not nix-build.
  4. If you are however using a self-hosted runner in Github Actions with public repos, read this [security warning](https://help.github.com/en/actions/automating-your-workflow-with-github-actions/about-self-hosted-runners#self-hosted-runner-security-with-public-repositories).

[kbd]: kbd:
