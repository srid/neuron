---
slug: tag-queries
---

Neuron supports *tag link queries* that can be used to *dynamically* create links based on a tag (see [[Tags]]).

For example, to link to all zettels with the "science" tag (from the example at [[Zettel metadata]]), you would add the following to your note:

```
[[z:zettels?tag=science&timeline]]
```

When neuron encounters an [URI] with the `z:` protocol such as the above, it treats it as a link query. The `z:zettels` query in particular will link to all zettels tagged with the tag specified in the `tag` query parameter.

:::{.ui .message}
**Tip**: You can use the CLI to see which zettels will be included in a given query; see [[Querying with JSON output]].
:::

## Demo

Here is a list of zettels tagged "walkthrough" on this very Zettelkasten:

[[z:zettels?tag=walkthrough]]

The above list was produced by the link query `[[z:zettels?tag=walkthrough]]`.

## Hierarchical tags

Queries can also link to zettels whose [[Tags]] match a glob pattern. Two kinds of patterns in particular are noteworthy:

1. **Simple globs**: Simple globs (`*`) can be used to query all tags at a particular level in the hierarchy. For instance, `[[z:zettels?tag=science/*]]` will link to all zettels tagged "science/physics" *and* "science/biology", but *not* "science/physics/kinematics".

2. **Recursive globs**: Recursive globs (`**`) are like simple globs, but they operate recursively, matching tags at *all* levels in the hierarchy.  For instance, `[[z:zettels?tag=science/**]]` will also match "science/physics/kinematics". This will also include zettels that are tagged "science" only, though this behavior can be avoided by querying "science/\*/\*\*" instead.

:::{.ui .message}
For a real-world example of link queries and hierarchical tags, see [Alien Psychology](https://alien-psychology.zettel.page/) ([source](https://github.com/srid/alien-psychology)).
:::

## Control flags

Link queries support a few query flags to control the link listing UI:

* `?grouped`: Group the results by matching tag (use with hierarchical tags)
* `?timeline`: Sort the results by `date` from [[Zettel metadata]], and also display the date.
* `?showid`: Display the zettel ID alongside the zettel title (link).

## Limit the amount of zettels

You can limit the amount of zettels to be shown in a query. This can be useful for e.g. a feed of posts.

```
[[z:zettels?tag=**&limit=2&timeline]]
```

## Limitations of tag queries

Non-ascii tags must be URI encoded when using in tag queries. See [this comment](https://github.com/srid/neuron/issues/446#issuecomment-720001775). Alternatively, if tagging in your case is semantically equivalent to linking, you may use [[Folgezettel Links]].

[URI]: https://en.wikipedia.org/wiki/Uniform_Resource_Identifier
