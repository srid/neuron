---
slug: editor
---

While you may use any text editor with neuron, the following extensions enable certain neuron-specific features on top of basic text editing.

## VSCode

:::{.ui .message}
VSCode is recommended for new users of neuron. Though you may also use other Zettelkasten apps that supports wiki-links (see [[Linking]]) like [Obsidian](https://obsidian.md/).
:::

Use the [vscode-memo](https://github.com/svsool/vscode-memo#memo) extension when editing your Neuron notes in [Visual Studio Code](https://code.visualstudio.com/). For other useful extensions, consult the template repo ([`.vscode/extensions.json`](https://github.com/srid/neuron-template/blob/master/.vscode/extensions.json)) in [[Automatic Publishing]].

![VSCode Gif Demo](./static/vscode-title-id.gif "Demo of editing neuron notes in VSCode"){.ui .centered .large .image}

## Vim/Neovim


- [neuron-v2.vim](https://github.com/chiefnoah/neuron-v2.vim) 
- [neuron.nvim](https://github.com/oberblastmeister/neuron.nvim) (neovim only)
- [nerveux.nvim](https://github.com/pyrho/nerveux.nvim) (neovim only)

## Emacs 

You can use one of the following modes to edit neuron notes in Emacs. `neuron-mode` supports more neuron-specific functionality than `markdown-mode`.

### Editing with `neuron-mode`

[neuron-mode](https://github.com/felko/neuron-mode) supports nifty editor features like opening a zettel by title, linking to other zettels by title, as well as displaying the title of the zettel next to the link (see screenshot below).

![screenshot](https://user-images.githubusercontent.com/3998/80873287-6fa75e00-8c85-11ea-9cf7-6e03db001d00.png){.ui .centered .large .image}

### Editing with `markdown-mode`

[markdown-mode](https://github.com/jrblevin/markdown-mode) may be desirable to those that want only basic Zettelkasten functionality. In order to be able to open wiki-links (`markdown-follow-thing-at-point`) you must use the following settings:

```elisp
(setq markdown-enable-wiki-links t)
(setq markdown-link-space-sub-char " ")
(setq markdown-wiki-link-search-type '(project))
```

## Editors known to work with v1

These editors are known to work with version 1 of neuron. Your mileage may vary with the latest development version (version 2) of neuron.

### Vim

See [this fork of neuron.vim](https://github.com/fiatjaf/neuron.vim).

![screenshot](https://github.com/fiatjaf/neuron.vim/raw/master/screenshot.png){.ui .centered .large .image}

### Online

[[Cerveau]] can be used to edit your neuron v1 notes online using a web browser. Here's a small demo of the Cerveau editor in action, demonstrating the link autocomplete feature.

![demo](./static/cerveau-autocompl.gif)
