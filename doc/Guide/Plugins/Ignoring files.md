---
slug: neuronignore
---

The `neuronignore` plugin allows you to specify a list of patterns that will be used to ignore specific Markdown files.

Add a file named `.neuronignore` to your notes directory. It should look like this:

```ini
# Ignore top-level dotfiles and dotdirs
.*/**

# Ignore project specific files
README.md
CHANGELOG.md
LICENSE.md

# Ignore everything under sub directories 
# (if not using dirtree plugin)
*/*/**
```