---
slug: plugins
---

Neuron includes several plugins, some of them are enabled by default.

You can turn on or turn off these plugins in [[configuration]]. For example, the following setting in your `neuron.dhall` will enable the [[Ignoring files]] plugin, while leaving others (such as [[Directory Tree]]) disabled.

```dhall
{ plugins = [ "neuronignore" ]
}
```

To enable *all* plugins,

```dhall
{ plugins = [ "neuronignore", "dirtree" ]
}
```

Available plugins are listed immediately below.