# Zettel ID

A Zettel ID is an unique identifier that refers to a particular zettel. By default, neuron will use random alphanumeric IDs of length 8, called a "random ID". But you may use arbitrary text as ID as well, called a "title ID".

## When to use *title IDs*

A title ID is one that uses arbitrary phrases (with whitespace characters). For example, in the link `[[Some note title]]` (see [[2011504]]), "Some note title" is the title ID, which is generated in the [[2011405]] as the HTML file named "Some_note_title.html".

Use title IDs when you want truly future-proof link IDs that work on any text editor. However, note that this comes at the cost that you are willing to rename them (manually or using a script) across your Zettelkasten if the title ID of any of your notes changes.

## When to prefer *random IDs*

The advantage to using random IDs (which neuron uses by default) is that you do not have to rename links across your Zettelkasten when changing the title IDs. This makes the links less future-proof, because you will want to use an editor (see [[4a6b25f1]]) that supports expanding them with the title from the note text. 