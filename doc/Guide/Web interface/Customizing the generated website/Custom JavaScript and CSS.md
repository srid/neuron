---
slug: custom-head
---

You can add custom JavaScript or CSS code to the `<head>` element of the generated pages by adding it to the `head.html` file under the notes directory. 

If this file does not exist, neuron will use its builtin one that provides

- MathJax JavaScript (for [[Math support]])
- Prism JavaScript (for [[Code Syntax Highlighting]])

:::{.ui .message}
Note that if you are going to specify a custom `head.html`, you must include the above manually. That is, copy-paste the following to your new `head.html`:

```html
<!-- MathJax -->
<script async="" id="MathJax-script" src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>
<!-- Prism.js -->
<link href="https://cdnjs.cloudflare.com/ajax/libs/prism/1.23.0/themes/prism.min.css" rel="stylesheet" />
<script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.23.0/components/prism-core.min.js"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.23.0/plugins/autoloader/prism-autoloader.min.js"></script>
```
:::

Here are some of the interesting things you can do with a custom `head.html`:

[[z:zettels?tag=custom-head-recipe]]#