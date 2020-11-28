# Customizing the generated website

Although neuron uses fixed HTML & CSS structure, you can customize the final site in one of the few ways.

## Favicon

A custom [favicon](https://en.wikipedia.org/wiki/Favicon) for your site can be specified by copying it to the `static` directory (see [[2016401]]). Neuron recognizes the following file names:

* `static/favicon.svg`
* `static/favicon.png`
* `static/favicon.ico`
* `static/favicon.jpg`
* `static/favicon.jpeg`
* `static/apple-touch-icon.png`

## Web app manifest

If a [web app manifest](https://web.dev/add-manifest/) file named `static/manifest.webmanifest` exists, Neuron will automatically use it in generated pages.

## Custom JavaScript / CSS

[[[custom-head]]]
