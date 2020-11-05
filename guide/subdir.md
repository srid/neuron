# Organizing notes in sub-directories

:::{.ui .warning .message}
This is an #experimental feature. Note that sub-directories are unsupported in [[cerveau]]. 
:::

There is an experimental support for asking neuron to look at notes recursively in the sub directories. [[id]] will still be determined from the filename of the notes, regardless of the directory path they are in.

To enable this feature:

```dhall
  recurseDir = True
```

## External links

- [Neuron issue \#309](https://github.com/srid/neuron/issues/309): ongoing discussion of this feature.