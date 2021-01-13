---
slug: dirtree
---

The *Directory Tree* plugin automatically creates a [[folgezettel-heterarchy]] reflecting the dirtectory tree of your notes. In effect, it does the following:

1. Create a "directory zettel" on the fly for each sub-directory containing zettels
2. Tag every zettel with a hierarchical [[tags]] (`root/**`) corresponding to its path
3. Create folgezettel links (see [[linking]]) automatically reflecting the directory tree

## Using and creating Directory Zettels

Given a file `./Home/Projects/HouseWarming.md` this plugin will create three zettels with [[id]]s `Home`, `Projects`, and `HouseWarming`. In the [[web]], neuron will display the "contents" of a directory beneath the zettel content.

Directory names must be **globally** unique across the entire zettelkasten, inasmuch they directly reflect [[id]] which must also be unique. This means that two directories or zettels with the same name but in different paths will not work: having both `./Home/Projects/HouseWarming` and `./Work/Projects/FireZeMissiles` will cause Neuron to attempt to generate two zettels with the ID `Projects`, which will cause an error. In order to resolve this error, and to keep the zettel IDs [[atomic]], it would be better to rename **both** notes to `./Home/HomeProjects/` and `./Work/WorkProjects/`.

### Adding content to a Directory Zettel

The zettel content for directory zettels are by default empty, only showing a list of links to its contents. However, you may explicitly specify them by creating zettel files using the same [[id]]; for the example above, you may create `./HomeProjects.md`- and it will be "merged" to the auto-created directory zettel:

```
# Directory structure

└─Home/
  ├── HomeProjects/        # HomeProjects **directory** zettel
  │   └── HouseWarming.md
  └── HomeProjects.md      # Note whose content will be merged into the HomeProjects **directory** zettel
```

```
# Generated HTML notes

- Home.html
- HomeProjects.html
- HouseWarming.html
```

No addtional zettels are generated, but any content in the `HomeProjects.md` file will be shown in the generated `HomeProjects.html` file in the [[web]].

## Directory Zettels are normal zettels

When Neuron is configured to use the **Directory Tree** plugin, it looks for all the markdown files in your repository, regardless of how many folders deep they are (unless you are using the [[Ignoring files]] plugin).

The directory structure you have on disk is used to do 2 things:

- automatically create a [[folgezettel-heterarchy]] from a directory zettel with the zettels for its contents
- automatically tag the zettels created with their on-disk path

Once Neuron generates the [[zettelkasten]], the directory structure is discarded from memory and not used in the [[web]]. All of the generated notes are made accessible at the 'top' level of the generated site--you don't need to navigate down the on-disk directory structure in the [[web]].

This is one reason why it's good practice to give your directories an [[atomic]] [[id]]--once you've generated your zettelkasten, you no longer have the parent directories around to provide context. The example from before, `./Home/Projects`, would create a note for your home projects with the ID `Projects`--we can no longer tell that it is specifically **home** projects. (The other reason it is good to have unique IDs is to avoid ID clash, as covered above.)

### Automatically created folgezettel heterarchies

Even though the on-disk directory structure is discarded, the information it carries is preserved by Neuron when it creates a [[folgezettel-heterarchy]] between a Directory Zettel and its child notes/contents.

Putting the `HomeProjects.md` zettel inside the `Home/` folder creates a parent-to-child folgezettel relationship from `Home` to `HomeProjects`, _as if you had a `Home.md` note that included a folgezettel link to `HomeProjects.md` in its content_.

This structure

```
└─Home/
  └── HomeProjects.md
```

creates a relationship equivalent to that created by these two notes, side by side, with a folgezettel link from the `Home.md` note to the `HomeProjects.md` note:

```
# ./Home.md
---
date: 2020-12-31
---

# Home

I'm working on some [[[HomeProjects]]] right now.
```

```
# ./HomeProjects.md
---
date: 2020-12-31
---

# Home Projects

- fix the leaky faucet
- paint the bathroom
```



#### The **Directory Tree** plugin only triggers on folders inside the Neuron zettelksaten

When enabled, the plugin will only work on the subfolders it finds **inside** the Neuron zettelkasten. This means that it will **not** automatically create a [[folgezettel-heterarchy]] from the `index` of your project to every zettel in the project (whether you have an explicit `index.md` zettel or just rely on Neuron's [[impulse-feature]] as your home)[^man].

### Creating links and heterarchies outside the directory

Directory Zettles just being normal zettels also means that you can link and create any [[folgezettel-heterarchy]] you like with the zettels created from directory trees. For example, we could create a new zettel called `Projects.md` at the 'top' of our Neuron zettelkasten, to gather together all of our more focused project zettels, as a sort of gateway:

```
# Directory structure

├── Projects.md
├── Work/
│   └── WorkProjects/
│      └── FireZeMissiles.md
└─ Home/
   ├── HomeProjects/
   │   └── HouseWarming.md
   └── HomeProjects.md
```

```
# ./Projects.md
---
date: 2020-12-31
tags:
  - work
  - home
---

# All Projects

Right now, my [[[WorkProjects]]] are taking most of
my focus and energy, and not leaving much time to focus
on my [[[HomeProjects]]].
```

### Automatically created tags

In addition to creating automatic a [[folgezettel-heterarchy]] for each directory, the plugin also [[tags]] the notes with their on-disk path, up to, but **not including** their own ID. So the note at `./Home/HomeProjects/HouseWarming.md` would get the **hierarchical** tag `#root/Home/HomeProjects`. These tags always start at the `#root` tag.

Given our work-and-home project:

```
├── Work/
│   └── WorkProjects/
│      └── FireZeMissiles.md
└─ Home/
   ├── HomeProjects/
   │   └── HouseWarming.md
   └── HomeProjects.md
```

notes would be generated with these hierarchical tags that match their folder
path:

| Note                  | Tag                       |
| --------------------- | ------------------------- |
| `Work.html`           | `#root`                   |
| `WorkProjects.html`   | `#root/Work`              |
| `FireZeMissiles.html` | `#root/Work/WorkProjects` |
| `Home.html`           | `#root`                   |
| `HomeProjects.html`   | `#root/Home`              |
| `HouseWarming.html`   | `#root/Home/HomeProjects` |

[^man]: You may manually form these relationships by adding `[[[z:zettels?tag=root]]]` to the top-level `index.md`, since all the generated Directory Zettels are tagged with `#root`

