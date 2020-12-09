# Contributing to neuron

This document describes how to develop neuron, as well as some guidelines when it comes to submiting a PR.

## Developing

We recommend [VSCode] for developing Neuron, but any text editor supporting [haskell-language-server] will do. When opening the project in VSCode, install the two extensions ([Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) and [Nix Environment Selector](https://marketplace.visualstudio.com/items?itemName=arrterian.nix-env-selector)) it recommends.

[VSCode]: https://code.visualstudio.com/
[haskell-language-server]: https://github.com/haskell/haskell-language-server#editor-integration

### Instant reload

When modifying the source code, use `bin/run` (which uses ghcid) to test your changes in real-time:

```bash
bin/run -d $(pwd)/guide rib -wS
```

This command automatically recompiles and restarts when you change any of the Haskell source files. Furthermore, this command runs site generation on the given Zettelkasten. You can pass the same neuron arguments to `bin/run`. This is essentially equivalent to running a development version of neuron with instant reload.

### Running tests

Unit tests can be run via ghcid as follows:

```
bin/test
```

This too reloads when any of the source files change.

### Hacking on Pandoc's HTML layout

Neuron delegates HTML rendering of the Pandoc AST to [reflex-dom-pandoc](https://github.com/srid/reflex-dom-pandoc). To hack on it, first [install nix-thunk](https://github.com/obsidiansystems/nix-thunk) and then:

```sh
# This will clone the git repo of reflex-dom-pandoc at dep/reflex-dom-pandoc
nix-thunk unpack dep/reflex-dom-pandoc

# Let's work on that repo
cd dep/reflex-dom-pandoc
```

Then you can try your changes with
```
# Run ghcid (using neuron's nix config)
nix-shell ../../shell.nix --run ghcid
```

Now as you edit the reflex-dom-pandoc sources, ghcid should give you compiler feedback. Once you are done with your changes, simply re-run neuron's ghcid or bin/run (see further above) and it should reflect your changes.

When you are done, commit your changes to reflex-dom-pandoc (presumably in a branch) and then `git push` it. Finally, you must "pack" the thunk and commit the changes to the neuron repo:

```sh
cd ../..  # Back to neuron
rm -rf dep/reflex-platform/dist-newstyle # cleanup build artifacts before packing
nix-thunk pack dep/reflex-dom-pandoc
git add dep/reflex-dom-pandoc
```

## Guidelines when submitting a PR

### Autoformatting

Run the `bin/format` script to auto-format your Haskell source changes using [ormolu](https://github.com/tweag/ormolu). You don't need to do this when using VSCode which is configured to auto-format on save.

### Test your build

Run `nix-build` with your changes to make sure that everything compiles, and the tests succeed.

#### Installing from source

`nix-build` will produce a binary under `./result/bin/neuron`. You can also install directly from source using `nix-env -if .`.
