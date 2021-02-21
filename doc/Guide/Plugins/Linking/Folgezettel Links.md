---
slug: folgezettel
---

Wiki-links (see [[Linking]]) may be labelled as "folgezettel" if you are writing a "structure note" (or hub note, or index note). Folgezettel links are used to curate a tree-like structure (termed [[folgezettel-heterarchy]]) for your zettelkasten, which structure is used in both [[Uplink Tree]] and [[impulse-feature]].

To mark a wiki-link as "folgezettel", put the `#` letter to either the end or the beginning of the link. The position of `#` indicates the *direction* of the folgezettel relationship.

```
# Programming paradigms

There are different kinds of programming paradigms:

* [[Functional Programming]]#
* [[Imperative Programming]]#
```

When `#` is placed at the end[^legacy], as illustrated above, the current zettel becomes the (structural) parent of the linked note. Let's look at the opposite example, 

```
# Haskell

Haskell is an example of a language with #[[Functional Programming]] paradigm.
```

Here, the `#` is placed at the *beginning* of the link; thus, the linked zettel ("Functional Programming") becomes the parent of the current zettel ("Haskell"). Notice that, conceptually, this is equivalent to tagging; i.e., `#[[Foo]]` is semantically equivalent to `#foo`. Thus, the use of folgezettel links can obviate [[Tags]] for most typical use cases.

:::{.ui .message}
:::{.header}
Take-away
:::
If you are just starting out, begin with plain wiki-links. Once your get comfortable, experiment with making some of them folgezettel, while confirming the desired heterarchy in [[impulse-feature]] or [[Uplink Tree]].
:::

[^legacy]: [[Neuron v1]] used `[[[..]]]` to create forward folgezettel links. These are still supported for backwards compatability, but users should use `[[..]]#` going forward.