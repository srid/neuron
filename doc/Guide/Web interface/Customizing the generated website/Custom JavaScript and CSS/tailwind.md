---
tags: [custom-head-recipe]
---

# Using Tailwind CSS classes

Neuron provides [Semantic UI](https://fomantic-ui.com/) already, however you can use other CSS toolkits as well. [Tailwind](https://tailwindcss.com/) in particular is a note-worthy one as it allows you to style elements using pre-defined *semantically defined* classes. 

First, activate Tailwind using [`twind/shim`](https://twind.dev/docs/modules/twind_shim.html), in your `head.html` (see [[Custom JavaScript and CSS]]):

```html
<!-- 
cf. https://github.com/tw-in-js/twind/discussions/161#discussioncomment-535632
-->
<script type="text/javascript" crossorigin="anonymous" src="https://unpkg.com/twind@0.16.9/twind.umd.js"> </script>
<script type="text/javascript" crossorigin="anonymous" src="https://unpkg.com/twind@0.16.9/observe/observe.umd.js"> </script>
<script type="text/javascript">
  twind.setup(
    {
      mode: twind.silent,  // Behave well with semantic UI classes
      preflight: false,  // Don't reset semantic UI!
    }
  );
  twindObserve.observe(document.documentElement)
</script>
<!-- End of Twind script -->
```

That's it; now you can use any of the Tailwind CSS classes in your Markdown files. Here's an example:

```markdown
## Highlights of the day:

:::{.rounded .shadow-2xl .border-2 .border-solid .border-pink-400 .text-xl .mb-4}
- Drank a new type of *coffee*
- Hacked a new feature to my pet project
- Enjoyed doing nothing in particular
:::

Random bits:
- These are not styled like the above div.
```

## Live example

See https://www.srid.ca/now (the pink box)