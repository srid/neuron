---
slug: custom-head
---

You can add custom JavaScript or CSS code to the `<head>` element of the generated pages by adding it to the `head.html` file under the notes directory. 

If this file does not exist, neuron will use its builtin one that provides

- MathJax JavaScript (see [[Math support]])
- Prism JavaScript (see [[Code Syntax Highlighting]])

:::{.ui .message}
Note that if you are going to specify a custom `head.html`, you must include the above manually.
:::

Here are some of the interesting things you can do with a custom `head.html`:

[[z:zettels?tag=custom-head-recipe]]#