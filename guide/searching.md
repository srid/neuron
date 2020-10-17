---
tags:
  - walkthrough
---

# Searching your Zettelkasten

## Interactive search

Use the `search` command to search for a particular zettel:

```bash
neuron search
```

This command[^1] will allow you to search your Zettels by title, and then print the matching zettel's filepath at the end. 

You may pipe the command to your text editor in order to directly edit the matching Zettel, or simply pass the `-e` option which opens the zettel in your $EDITOR:

```bash
neuron search -e
```

See asciinema: <https://asciinema.org/a/313358>

![asciicast](https://asciinema.org/a/313358.png)

### Full-text search

The `--full-text` (alias: `-a`) option can be used to search by the whole content, not just the title:

```bash
neuron search -a
```
[^1]: Internally, `neuron search` uses the tool [`bat`](https://github.com/sharkdp/bat) to display the selected Zettel. Note that you might want to configure `bat`, such as to set the color theme (the default `bat` theme is for a dark terminal background, which might not work well on light-themed terminals).
