# Link Queries

Neuron supports *link queries* that can be used to *dynamically* create links based on a tag (see [[tags]]).

For example, to link to all zettels with the "science" tag (from the example at [[metadata]]), you would add the following to your note:

```
[[z:zettels?tag=science&timeline]]
```

When neuron encounters an [URI] with the `z:` protocol such as the above, it treats it as a link query. The `z:zettels` query in particular will link to all zettels tagged with the tag specified in the `tag` query parameter.

:::{.ui .message}
**Tip**: You can use the CLI to see which zettels will be included in a given query; see [[query]].
:::

## Demo

Here is a list of zettels tagged "walkthrough" on this very Zettelkasten:

[[z:zettels?tag=walkthrough]]

The above list was produced by the link query `[[z:zettels?tag=walkthrough]]`.[^folge]

See [[experimental]] for a real-world example of link queries on this site.

## Hierarchical tags

Queries can also link to zettels whose [[tags]] match a glob pattern. Two kinds of patterns in particular are noteworthy:

1. **Simple globs**: Simple globs (`*`) can be used to query all tags at a particular level in the hierarchy. For instance, `[[z:zettels?tag=science/*]]` will link to all zettels tagged "science/physics" *and* "science/biology", but *not* "science/physics/kinematics".

2. **Recursive globs**: Recursive globs (`**`) are like simple globs, but they operate recursively, matching tags at *all* levels in the hierarchy.  For instance, `[[z:zettels?tag=science/**]]` will also match "science/physics/kinematics". This will also include zettels that are tagged "science" only, though this behavior can be avoided by querying "science/\*/\*\*" instead.

:::{.ui .message}
For a real-world example of link queries and hierarchical tags, see [Alien Psychology](https://alien-psychology.zettel.page/) ([source](https://github.com/srid/alien-psychology)).
:::

## Control flags

Link queries support a few query flags to control the link listing UI:

* `?grouped`: Group the results by matching tag (use with hierarchical tags)
* `?timeline`: Sort the results by `date` from [[metadata]], and also display the date.
* `?showid`: Display the zettel ID alongside the zettel title (link).

## Limit the amount of zettels

You can limit the amount of zettels to be shown in a query. This can be useful for e.g. a feed of posts.

[[z:zettels?tag=**&limit=2&timeline]]

[URI]: https://en.wikipedia.org/wiki/Uniform_Resource_Identifier

[^folge]: Note that here we use `[[..]]` to not affect the [[folgezettel-heterarchy]] of the graph; whereas if we had used `[[[...]]]`, it would have formed the appropriate folgezettel connections to the listed notes (which is not what we want on *this* note).
