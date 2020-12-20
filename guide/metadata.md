# Zettel metadata

Zettels may contain optional metadata in the YAML frontmatter.

## Date

The date of the zettels can be specified in the "date" metadata field (`neuron new` automatically fills in this field):

```markdown
---
date: 2020-08-21T13:06
---
```

This date can be made to display in a query result by using the `timeline` flag (see [[link-query]]).

## Slug

The "slug" of a zettel is used in its URL, which in turn is determined by the filename of the generated HTML file. By default neuron will use the [[id]] with whitespace replaced with underscore as the slug, which may be overriden here.

```markdown
---
slug: foo-bar
---
```

## Pinning

Zettels can be pinned in [[impulse-feature]] so that they appear at the top. To pin a zettel, add the "pinned" tag (see [[tags]]) to it.

```markdown
---
tags:
  - pinned 
---
```

## Hiding a zettel

Sometimes you want to "draft" a note, and as such wish to hide it from the rest of Zettelkasten, notably in [[impulse-feature]]. This can be achieved by marking a zettel as "unlisted":

```markdown
---
unlisted: true 
---
```

## Other metadata 

You can explicitly specify a title using the `title` metadata; otherwise, Neuron will infer it from the Markdown heading or [[id]].

The metadata key `tags` or `keywords` can be used to specify tags, although neuron supports inline tags as well (see [[[tags]]]).

