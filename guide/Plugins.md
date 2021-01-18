---
slug: plugins
---

Neuron includes several plugins, some of them are enabled by default.

You can turn on or turn off these plugins in [[configuration]]. For example, the following setting in your `neuron.dhall` will enable the default plugins, while leaving others (such as [[Directory Tree]]) disabled.

```dhall
{ plugins = [ "links", "tags", "neuronignore" ]
}
```

To enable *all* plugins,

```dhall
{ plugins = [ "links", "tags", "neuronignore", "dirtree" ]
}
```

Available plugins are listed immediately below.