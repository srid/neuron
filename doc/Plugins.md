---
slug: plugins
---

Neuron includes several plugins, some of them are enabled by default.

You can turn on or turn off these plugins in [[Configuration file]]. For example, the following setting in your `neuron.dhall` will enable the three specified plugins, while leaving everything else disabled:

```dhall
{ plugins = [ "neuronignore", "links", "uptree" ]
}
```

List of available plugins is displayed below:

| Plugin Name    | Documentation      | Enabled by default? |
|----------------|--------------------|---------------------|
| `neuronignore` | [[Ignoring files]] | Yes                 |
| `links`        | [[Linking]]        | Yes                 |
| `tags`         | [[Tags]]           | Yes                 |
| `uptree`       | [[Uplink Tree]]    | Yes                 |
| `dirtree`      | [[Directory Tree]] | No                  |
