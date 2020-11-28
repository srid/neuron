---
tags: [custom-head-recipe]
---

# Graphviz

[Graphviz](https://graphviz.org/) is visualization software for representing structural information like abstract graphs and networks.

Render Graphviz content in neuron by including the following HTML snippet in your `head.html` file ([[custom-head]]) or the zettel file ([[raw-html]]):

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/viz.js/2.1.2/viz.js" integrity="sha512-vnRdmX8ZxbU+IhA2gLhZqXkX1neJISG10xy0iP0WauuClu3AIMknxyDjYHEpEhi8fTZPyOCWgqUCnEafDB/jVQ==" crossorigin="anonymous"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/viz.js/2.1.2/full.render.js" integrity="sha512-1zKK2bG3QY2JaUPpfHZDUMe3dwBwFdCDwXQ01GrKSd+/l0hqPbF+aak66zYPUZtn+o2JYi1mjXAqy5mW04v3iA==" crossorigin="anonymous"></script>
<script>
window.addEventListener("load", function(){
  let viz = new Viz();
  for (let element of document.getElementsByClassName("graphviz")) {
    let parent = element.parentNode
    let pparent = parent.parentNode
    viz.renderSVGElement(element.textContent)
    .then(function(element) {
      element.setAttribute("width", "100%")
      pparent.replaceChild(element, parent)
    });
  }
});
</script>
```

Then include your Graphviz syntax in Markdown as follows:

~~~markdown
```{.graphviz}
digraph G {

	subgraph cluster_0 {
		style=filled;
		color=lightgrey;
		node [style=filled,color=white];
		a0 -> a1 -> a2 -> a3;
		label = "process #1";
	}

	subgraph cluster_1 {
		node [style=filled];
		b0 -> b1 -> b2 -> b3;
		label = "process #2";
		color=blue
	}
	start -> a0;
	start -> b0;
	a1 -> b3;
	b2 -> a3;
	a3 -> a0;
	a3 -> end;
	b3 -> end;

	start [shape=Mdiamond];
	end [shape=Msquare];
}
```
~~~

When you open your generated neuron site, it will render as follows:

```{.graphviz}
digraph G {

	subgraph cluster_0 {
		style=filled;
		color=lightgrey;
		node [style=filled,color=white];
		a0 -> a1 -> a2 -> a3;
		label = "process #1";
	}

	subgraph cluster_1 {
		node [style=filled];
		b0 -> b1 -> b2 -> b3;
		label = "process #2";
		color=blue
	}
	start -> a0;
	start -> b0;
	a1 -> b3;
	b2 -> a3;
	a3 -> a0;
	a3 -> end;
	b3 -> end;

	start [shape=Mdiamond];
	end [shape=Msquare];
}
```

<!-- Usually this goes to head.html, but we include it here because we don't want the JS to run on other note files. -->
<script src="https://cdnjs.cloudflare.com/ajax/libs/viz.js/2.1.2/viz.js" integrity="sha512-vnRdmX8ZxbU+IhA2gLhZqXkX1neJISG10xy0iP0WauuClu3AIMknxyDjYHEpEhi8fTZPyOCWgqUCnEafDB/jVQ==" crossorigin="anonymous"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/viz.js/2.1.2/full.render.js" integrity="sha512-1zKK2bG3QY2JaUPpfHZDUMe3dwBwFdCDwXQ01GrKSd+/l0hqPbF+aak66zYPUZtn+o2JYi1mjXAqy5mW04v3iA==" crossorigin="anonymous"></script>
<script>
window.addEventListener("load", function(){
  let viz = new Viz();
  for (let element of document.getElementsByClassName("graphviz")) {
    let parent = element.parentNode
    let pparent = parent.parentNode
    viz.renderSVGElement(element.textContent)
    .then(function(element) {
      element.setAttribute("width", "100%")
      pparent.replaceChild(element, parent)
    });
  }
});
</script>

