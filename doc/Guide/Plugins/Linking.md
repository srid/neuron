# Linking

To link to another zettel, you can use regular Markdown links, or use the
special Wiki-link syntax by putting the [[id]] inside `[[..]]`:

```
This is a zettel file, which links to another zettel:

* [[New Discourses]]

You can also use regular Markdown links:

* [new discourse](New Discourses.md)
```

In [[Web interface]], neuron will automatically display the title of the
linked zettel when using the wiki-link syntax.

## Branching links

Neuron supports branching links, which can be created using `[[[..]]]`:

```
This is a zettel file, which links (branches of) to another zettel:

* [[[New Discourses]]]
```

These links are called a folgezettel link. They are like regular wiki-links discussed previously, except for the "folgezettel" label enabled by the 3-bracket syntax. When a zettel has a folgezettel relationship to another zettel, it is said to "branch of" (categorically) to the other zettel. Folgezetel relationships define the [[folgezettel-heterarchy]] of your zettel graph. 

Branching links affect the linked zettel's [[Uplink Tree]] where the linking zettel is displayed.

In short, use folgezettel links to curate the "structure" of your zettelkasten. When in doubt, use regular wiki-links, and then upgrade them to be folgezettel if necessary.

## Advanced linking

If you use the [[Tags]] plugin, you can link automatically based on the tag (see [[[Tag Queries]]]).
