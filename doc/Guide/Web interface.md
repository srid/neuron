---
slug: web
---

Neuron can generate a fully-functional and self-sufficient web site out of your zettelkasten. It generates the HTML files under your Zettelkasten directory, in `.neuron/output/`, as well as spin up a server that will serve that generated site at [localhost:8080](http://localhost:8080).

```bash
neuron gen -wS
```

The `gen` command takes a few options, notably:

* You can override the output directory path using `-o`.

* You can override server settings such as the host and port. For example,

    ```bash
    neuron gen -ws 127.0.0.1:8081
    ```

Additional CLI details are available via `--help`.

## Local site without server

The web interface can also be accessed without necessarily running the server.
First run neuron generator in "watch mode" only (no http server):

```bash
# Watch only, without serving
neuron gen -w
```

Leave this command running in one terminal, and then use `neuron open` to directly open the locally generated HTML site.

:::{.ui .warning .message}
When using `neuron open` to access the generated site *locally*, do note that [[impulse-feature]] will not function, due to web browser security restrictions. The static version of Impulse will however work. The dynamic version of Impulse will work when your static site is served by a HTTP server, such as `neuron gen -s`.
:::

## Publishing to the web

See [[[Automatic Publishing]]]

## Features 

* [[[configuration]]]
* [[[2014601]]]
* [[[Customizing the generated website]]]
* [[[Graph visualization]]]

