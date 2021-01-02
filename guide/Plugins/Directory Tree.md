---
slug: dirtree
---

The *Directory Tree* plugin automatically creates a [[folgezettel-heterarchy]] reflecting the dirtectory tree of your notes. In effect, it does the following:

1. Create a "directory zettel" on the fly for each sub-directory containing zettels
2. Tag every zettel with a hierarchical [[tags]] (`root/**`) corresponding to its path
3. Create folgezettel links (see [[linking]]) automatically reflecting the directory tree

## Directory Zettel

Given a folder `./Project/HouseWarming/`, this plugin will create two zettels with [[id]] `Project` and `HouseWarming`. In the [[web]], neuron will display the "contents" of a directory beneath the zettel content.

Directory names must be **globally** unique across the entire zettelkasten, inasmuch they directly reflect [[id]] which must also be unique. This means that two directories or zettels with the same name but in different paths will not work: having both `family/notes` and `work/notes` will cause Neuron to attempt to generate two zettels with the ID `notes`, which will cause an error. In order to resolve this error, and to keep the zettel IDs [[atomic]], it would be better to rename **both** notes to `work/work-notes/` and `family/family-notes`.

The zettel content for directory zettels are by default empty. However, you may explicitly specify them by creating zettel files using the same [[id]]; for the example above, you may create `./Project.md` and `./Project/HouseWarming.md` - and they will be "merged" to the auto-created directory zettels.

### Generated notes are 'lifted' out of their directory hierarchy

Notes whose files are located in a subdirectory, will be 'lifted' to the same level as the 'root'--at the same level as their parents and grandparents.

For example, the following directory structure

```
└─programming
  ├── ruby.md
  ├── ruby
  │   ├── rails.md
  │   └── rspec-testing.md
  └── functional-programming.md
```

would create the following HTML documents, all at the top-level of the generated Neuron site: 

- `programming.html`
- `ruby.html`
- `rails.html`
- `rspec-testing.html`
- `functional-programming.html`

Because a directory's name will become its zettel ID, and because all zettels are 'lifted' out of their path hierarchy when notes are generated, it is important to give it a clear and meaningful name (if you are using non-random IDs). 

A zettel created by the directory `work/notes` is a bad zettel ID, because once at the 'top' level of the zettelkasten, the ID `notes` has lost the context provided by its parent directory. A better ID would be `work/work-notes`.

While this might seem redundant at first glance, the rationale for this this lift is to keep the zettels [[atomic]], and to keep linking as simple as possible. To link to the `work-notes` zettel, you don't need to remember that it's in the `work/` directory--you just link to `[[work-notes]]`. And if you choose to move it in the future, you don't need to hunt down and remember to update links that have been made to it.

## Links & tags created

A [[folgezettel-heterarchy]] is formed, reflecting the directory tree on disk, with the exception of the `index` zettel[^man]. The forming of links is based on hierarchical [[tags]] (reflecting the relative path), that every normal zettel and directory zettel is automatically tagged with.

[^man]: You may manually form these relationships by adding `[[[z:zettels?tag=root]]]` to the top-level `index.md`

