---
slug: create
tags:
  - walkthrough
---

# Creating and Editing zettels

You may use any text editor with Markdown support to edit your zettel files. Neuron provides a command to create new zettel files with the suitable [[Zettel ID]]:

```bash
neuron new
```

This command will print the path to the file created. Use `-e` to also open the text editor:

```bash
neuron new -e
```

Of course, you can also start with an empty file and begin writing:

```bash
echo "Hello world ..." > "My new note.md"
```

Do not forget to link your new zettel to the rest of your Zettelkasten. See [[Linking]].

## Opening a Zettel by title

See [[Searching your Zettelkasten]].

## Using a text editor

See [[Editor integration]]#

## Web Interface

[[Cerveau]]# provides a web interface to browse and edit your Neuron v1 notes.
