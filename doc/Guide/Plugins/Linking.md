# Linking

Although you may use regular Markdown links, neuron supports **wiki-links** that are often more convenient to use to link zettels. To link to a zettel using wiki-links, place that zettel's [[id]] inside `[[..]]`. For example,

```
[New Discourses][nd] is an excellent resource for 
debunking [[Critical Race Theory]] (aka. CRT).

[nd]: https://newdiscourses.com/translations-from-the-wokish/
```

In [[Web interface]], neuron will automatically display the title of the
linked zettel.

## Folgezettel links

Wiki-links may be labelled as "folgezettel" if you are writing a "structure note" (or hub note, or index note). See [[Folgezettel Links]].

## Advanced linking

If you are using the [[Tags]] plugin, you can link to multiple zettels matching a tag (see [[Tag Queries]]#).
