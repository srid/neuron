---
slug: markdown
---

Zettel files are written in Markdown, per [CommonMark](https://commonmark.org/) and [GFM](https://github.github.com/gfm/) spec, along with most of [commonmark extensions](https://github.com/jgm/commonmark-hs/tree/master/commonmark-extensions) enabled.[^tech]

* [[Linking]]#
* [[metadata]]#
* [[2016401]]#
* Styling elements using Semantic UI ([\#176](https://github.com/srid/neuron/issues/176))


Enriching Markdown:

* [[raw-html]]#
* [[2013702]]#
* [[math]]#
* **Highlighting**: Surround text with `== ... ==` to highlighting them (eg: This ==word== is highlighted).
* **Footnotes**: Footnotes can be introduced by adding `[^someId]` to the end of text, and then adding (usually at the end of the markdown file) `[^someId]: The footnote's Markdown goes here`. For example:
    ```markdown
    Hopelessness is one of the big products of the *race industry*;[^sowell]
    you have to produce that kind of feeling in order serve the interests of
    those in the race industry.
    
    [^sowell]: Refer to [Thomas Sowell's views on "Black Lives
    Matter"](https://www.youtube.com/watch?app=desktop&v=Ap8FfZTgECY) which
    provides a real-world example to this affective setting-the-stage
    phenomemon.
    
    ```

[^tech]: Neuron uses [commonmark-hs](https://github.com/jgm/commonmark-hs) to parse them into the [Pandoc AST](https://pandoc.org/using-the-pandoc-api.html), as well as provides an extention on top to handle zettel links.
