---
tags: [custom-head-recipe]
---

# Mermaid

[Mermaid](https://mermaid-js.github.io/mermaid/) provides markdownish syntax for generating flowcharts, sequence diagrams, class diagrams, gantt charts and git graphs. To 

Render mermaid content in neuron by including the following HTML snippet in your `head.html` file ([[Custom JavaScript and CSS]]) or the zettel file ([[raw-html]]):

```html
<script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
<script>
window.addEventListener("load", mermaid.initialize({startOnLoad:true}))
</script>
```

Then include your Mermaid syntax in Markdown as follows:

~~~markdown
```{.mermaid}
sequenceDiagram
    Alice->>John: Hello John, how are you?
    activate John
    John-->>Alice: Great!
    deactivate John
```
~~~

When you open your generated neuron site, it will render as follows:

```{.mermaid}
sequenceDiagram
    Alice->>John: Hello John, how are you?
    activate John
    John-->>Alice: Great!
    deactivate John
```

<!-- Usually this goes to head.html, but we include it here because we don't want the JS to run on other note files. -->
<script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
<script>
window.addEventListener("load", mermaid.initialize({startOnLoad:true}))
</script>

