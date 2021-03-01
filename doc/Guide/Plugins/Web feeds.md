---
slug: feed
---

![](https://upload.wikimedia.org/wikipedia/en/thumb/4/43/Feed-icon.svg/1920px-Feed-icon.svg.png){.ui .right .floated .tiny .image}

The feed plugin is used to generate a [web feed] on a per-zettel basis. These zettels are generally considered to be hub notes or structure notes, because the feed's contents will be their direct folgezettel children (created using [[Folgezettel Links]]) with a `date` [[Zettel metadata]] property.

To activate feed generation for a zettel, add the following to its YAML frontmatter:

```yaml
---
feed:
  count: 5
---
```

The generated feed will be of the filename `${slug}.xml` (it is an Atom feed), where "slug" corresponds to the slug of the zettel (see [[Zettel metadata]]).

Feed items are determined from the graph, by looking for the folgezettel children of the zettel, which may be established in one of the following ways:

- [[Tag Queries]] with folgezettel label ([example](https://www.srid.ca/microblog))
- Wiki-links using [[Folgezettel Links]] ([example](https://www.srid.ca/blog))
- Adding to a subfolder, with the [[Directory Tree]] plugin enabled

A link to the feed will be added to the `<head>` element of the generated HTML.

[web feed]: https://en.wikipedia.org/wiki/Web_feed