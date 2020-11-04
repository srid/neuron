# Using Org Mode format

:::{.ui .warning .message}
This is an #experimental feature. Neuron recommends [[markdown]], which is supported everywhere including [[cerveau]]. 
:::

Neuron is architected to be extended with other markup formats as well. [Org Mode](https://orgmode.org/) is currently supported, in an #experimental fashion as not all features work with it. 

To enable support for org mode format, set the following in your [[configuration]] file:

```dhall
  formats = [ "markdown", "org" ]
```