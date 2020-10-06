# Zettel metadata

Zettels may contain optional metadata in the YAML frontmatter.

## Date

The date of the zettels can be specified in the "date" metadata field (`neuron new` automatically fills in this field):

```markdown
---
date: 2020-08-21T13:06
---
```

This date can be made to display in a query result by using the `timeline` flag (see [[2011506]]).

## Pinning

Zettels can be pinned in the z-index so that they appear at the top. To pin a zettel, add the "pinned" tag (see [[tags]]) to it.

```markdown
---
tags:
  - pinned 
---
```

## Hiding a zettel

Sometimes you want to "draft" a note, and as such wish to hide it from the rest of Zettelkasten, notably the z-index. This can be achieved by marking a zettel as "unlisted":

```markdown
---
unlisted: true 
---
```

## Other metadata 

You can explicitly specify a title using the `title` metadata; otherwise, Neuron will infer it from the Markdown heading or [[id]].

The metadata key `tags` or `keywords` can be used to specify tags, although neuron supports inline tags as well (see [[[tags]]]).

