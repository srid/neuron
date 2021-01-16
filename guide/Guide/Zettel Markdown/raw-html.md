# Using raw HTML in Markdown

You can write raw HTML inline in your Markdown notes. You can also used fenced code blocks to specify them explicitly.

### Using fenced code blocks

The Haskell CommonMark interpreter supports ['raw-attributes' to cause the code in the block to be interpreted as inline](https://github.com/jgm/commonmark-hs/blob/master/commonmark-extensions/test/raw_attribute.md).

From the docs:

> If attached to a fenced code block, it causes the block to be interpreted as raw block content with the specified format.

For HTML, this just requires adding `{=html}` after the opening ` ``` ` code block fence.

So, the following markdown content:

```````````````````````````````` markdown
## Some markdown interspersed with HTML

Here is a video:

``` {=html}
<video><source src='static/video.mp4' /></video>
```
````````````````````````````````

results in the following generated HTML:

```````````````````````````````` html
<h2>Some markdown interspersed with HTML</h3>
<p>Here is a video:</p>
<video><source src='static/video.mp4' /></video>
````````````````````````````````