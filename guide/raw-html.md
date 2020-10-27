# Using raw HTML in Markdown

Usually, Markdown supports [raw HTML inline as valid Markdown syntax](https://spec.commonmark.org/0.28/#raw-html).

Because Neuron previously used an angle-bracket syntax for linking (`<note>` would generate a link to `note.md`), when Neuron encounters a simple HTML tag in a note, it is parsed as if it were a link. This link format is now deprecated, but is still supported (see footnote on [[linking]]).
This may be changed in the future if a reliable migration path is identified (i.e. a script that would automatically convert angle-bracket links to wiki-links).

## Getting HTML to work

In order to get raw HTML to work in a note, for example to embed a `<video />` element, there are two options:

1. Wrap the HTML in a code-block with a 'raw-attribute', which causes the code block to be interpreted as raw inline content
2. Add an HTML attribute to your element, like `id`, `class` or `name`, which causes Neuron to recognize that element is not a note link

### Using fenced code blocks

The Haskell CommonMark interpreter supports ['raw-attributes' to cause the code in the block to be interpreted as inline](https://github.com/jgm/commonmark-hs/blob/master/commonmark-extensions/test/raw_attribute.md).

From the docs:

> If attached to a fenced code block, it causes the block to be interpreted as raw block content with the specified format.

For HTML, this just requires adding `{=html}` after the opening ` ``` ` code block fence.

So, the following markdown content:

```````````````````````````````` example
## Some markdown interspersed with HTML

Here is a video:

``` {=html}
<video><source src='static/video.mp4' /></video>
```
````````````````````````````````

results in the following generated HTML:

```````````````````````````````` example
<h2>Some markdown interspersed with HTML</h3>
<p>Here is a video:</p>
<video><source src='static/video.mp4' /></video>
````````````````````````````````

### Using HTML attributes

If the top-level HTML element has one or more attributes, neuron will never
parse it as a link.

This will likely happen without notice for elements like `iframe` (say, a Youtube embed), since they depend heavily attribute values. For simpler elements, something as small as adding an `id` works:


```````````````````````````````` example
## Some markdown interspersed with HTML

Here is a video:

<video id="a-video"><source src='static/video.mp4' /></video>

````````````````````````````````

results in the following generated HTML:

```````````````````````````````` example
<h2>Some markdown interspersed with HTML</h3>
<p>Here is a video:</p>
<video id="a-video"><source src='static/video.mp4' /></video>

````````````````````````````````

If you have a deeply nested HTML structure, the attribute is only required at the top/root level:

```````````````````````````````` example
## Some markdown interspersed with HTML

Here comes some nested HTML:

<div id="nested-content">
  <p>
    Some HTML content, <em>now with emphasis.</em> And <strong>now, very strong.</strong>
  </p>
</div>

````````````````````````````````
