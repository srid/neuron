# Editor integration

While you may use any text editor with neuron, the following extensions enable certain neuron-specific features on top of basic text editing.

## VSCode

:::{.ui .message}
VSCode is recommended for new users of neuron.
:::

Use the [vscode-memo](https://github.com/svsool/vscode-memo#memo) extension when editing your Neuron notes in [Visual Studio Code](https://code.visualstudio.com/). For other useful extensions, consult the template repo ([`.vscode/extensions.json`](https://github.com/srid/neuron-template/blob/master/.vscode/extensions.json)) in [[Automatic Publishing]].

![demo](./static/vscode-title-id.gif){.ui .centered .large .image}

## Editors known to work with v1

These editors are known to work with version 1 of neuron. Your mileage may vary with the latest development version (version 2) of neuron.

### Emacs 

Emacs support is available via [neuron-mode](https://github.com/felko/neuron-mode), which supports nifty editor features like opening a zettel by title, linking to other zettels by title, as well as displaying the title of the zettel next to the link (see screenshot below).

![screenshot](https://user-images.githubusercontent.com/3998/80873287-6fa75e00-8c85-11ea-9cf7-6e03db001d00.png){.ui .centered .large .image}

### Vim

See [this fork of neuron.vim](https://github.com/fiatjaf/neuron.vim).

![screenshot](https://github.com/fiatjaf/neuron.vim/raw/master/screenshot.png){.ui .centered .large .image}

### Online

[[cerveau]] can be used to edit your neuron v1 notes online using a web browser. Here's a small demo of the Cerveau editor in action, demonstrating the link autocomplete feature.

![demo](./static/cerveau-autocompl.gif)
