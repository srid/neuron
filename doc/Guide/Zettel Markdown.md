---
slug: markdown
---

Zettel files are written in Markdown, per [CommonMark](https://commonmark.org/) and [GFM](https://github.github.com/gfm/) spec, along with most of [commonmark extensions](https://github.com/jgm/commonmark-hs/tree/master/commonmark-extensions) enabled.[^tech]

* [[[Linking]]]
* [[[metadata]]]
* [[[2013702]]]
* [[[math]]]
* [[[2016401]]]
* Styling elements using Semantic UI ([\#176](https://github.com/srid/neuron/issues/176))
* [[[raw-html]]]

Misc features:

* Text highlighting: Surround text with `== ... ==` to highlighting them (eg: This ==word== is highlighted).

[^tech]: Neuron uses [commonmark-hs](https://github.com/jgm/commonmark-hs) to parse them into the [Pandoc AST](https://pandoc.org/using-the-pandoc-api.html), as well as provides an extention on top to handle zettel links.
