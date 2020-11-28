---
title: "mermaid-js"
---

<script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
<script>
window.addEventListener("load", mermaid.initialize({startOnLoad:true}))
</script>

[mermaid-js](https://mermaid-js.github.io/mermaid/#/)

```{.mermaid}
sequenceDiagram
    Alice->>John: Hello John, how are you?
    activate John
    John-->>Alice: Great!
    deactivate John
```

HTML
```html
<script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
<script>
window.addEventListener("load", mermaid.initialize({startOnLoad:true}))
</script>
```

Markdown
~~~markdown
```{.mermaid}
sequenceDiagram
    Alice->>John: Hello John, how are you?
    activate John
    John-->>Alice: Great!
    deactivate John
```
~~~
