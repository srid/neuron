---
slug: dirtree
---

The *Directory Tree* plugin achieves two things:

1. Create a "directory zettel" on the fly for each sub-directory
2. Create folgezettel links (see [[linking]]) automatically reflecting the directory tree

## Directory Zettel

Given a folder `./Project/HouseWarming/`, this plugin will create two zettels with [[id]] `Project` and `HouseWarming`. Directory names must be unique, inasmuch they directly reflect [[id]] which must also be unique. In the [[web]], neuron will display the "contents" of a directory beneath the zettel content. 

The zettel content for directory zettels are by default empty. However, you may explicitly specify them by creating zettel files using the same [[id]]; for the example above, you may create `./Project.md` and `./Project/HouseWarming.md` - and they will be "merged" to the auto-created directory zettels.

## Links & tags created

A [[folgezettel-heterarchy]] is formed, reflecting the directory tree on disk, with the exception of the `index` zettel[^man]. The forming of links is based on hierarchical [[tags]] (reflecting the relative path), that every normal zettel and directory zettel is automatically tagged with.

[^man]: You may manually form these relationships by adding `[[[z:zettels?tag=index]]]` to the top-level `index.md`