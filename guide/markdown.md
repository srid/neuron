# Zettel Markdown

Zettel files are written in Markdown[^other], per the [CommonMark](https://commonmark.org/) specification. Neuron uses [commonmark-hs](https://github.com/jgm/commonmark-hs) to parse them into the [Pandoc AST](https://pandoc.org/using-the-pandoc-api.html), as well as provides an extention on top to handle zettel links.

* [[[linking]]]
* [[[tags]]]
* [[[metadata]]]
* [[[2013702]]]
* [[[math]]]
* [[[2016401]]]
* Styling elements using Semantic UI ([\#176](https://github.com/srid/neuron/issues/176))
* [[[raw-html]]]

[^other]: Neuron is designed to be extended with other markup formats as well. Org-mode is currently supported (see the `formats` setting in [[configuration]]) but not all features work with it. Neuron recommends Markdown, which is supported everywhere including [[cerveau]].  
