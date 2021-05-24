---
slug: linking
---

Although you may use regular Markdown links, neuron supports **wiki-links** that are often more convenient to use to link zettels. To link to a zettel using wiki-links, place that zettel's [[Zettel ID]] inside `[[..]]`. For example,

```markdown
Place that zettel's [[id]] inside `[[..]]`. 
```

In [[Web interface]], neuron will automatically display the title of the
linked zettel.

Alternate link text can also be given using a pipe, as in `[[link|text to display]]`.

## Folgezettel links

Wiki-links may be labelled as "folgezettel" if you are writing a "structure note" (or hub note, or index note). See [[Folgezettel Links]].

## Advanced linking

If you are using the [[Tags]] plugin, you can link to multiple zettels matching a tag (see [[Tag Queries]]#).
