---
slug: metadata
---

Zettels may contain optional metadata in the YAML frontmatter.

## Date

The date of the zettels can be specified in the "date" metadata field. The time part is optional.

```yaml
---
date: 2020-08-21T13:06
---
```

Date is used in a number of places in neuron:

-  `neuron new` automatically fills in the date field.
- [[Tag Queries]]'s `timeline` flag displays the date in zettel listing.
- Date is used in [[Web feeds]]

## Slug

The "slug" of a zettel is used in its URL, which in turn is determined by the filename of the generated HTML file. By default neuron will use the lowercase version of [[Zettel ID]], with whitespace replaced with hyphen as the slug, which may be overriden here.

```yaml
---
slug: foo-bar
---
```

## Pinning

Zettels can be pinned in [[impulse-feature]] so that they appear at the top. To pin a zettel, add the "pinned" tag (see [[Tags]]#) to it.

```yaml
---
tags:
  - pinned 
---
```

## Hiding a zettel

Sometimes you want to "draft" a note, and as such wish to hide it from the rest of Zettelkasten, notably in [[impulse-feature]]. This can be achieved by marking a zettel as "unlisted":

```yaml
---
unlisted: true 
---
```

## Other metadata 

You can explicitly specify a title using the `title` metadata; otherwise, Neuron will infer it from the Markdown heading or [[Zettel ID]].

The metadata key `tags` or `keywords` can be used to specify tags, although neuron supports inline tags as well (see [[Tags]]#).
