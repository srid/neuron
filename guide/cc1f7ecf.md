---
date: "2020-06-26"
---

# Querying with JSON output

Use the `query` command to query your Zettelkasten and return the matches in JSON format. 

```bash
# Returns all zettels
neuron query
```

```bash
# Returns zettels with the specified tag
neuron query -t science
```

You may also pass the same URI you use in <2011506>:

```bash
# Search using link URI
neuron query --uri="z:zettels?tag=science"
```

Use `jq` to extract needed information from the JSON output. For example, to
extract only the IDs:

```bash
$ neuron query -t purescript | jq -r '.result | .[] | .zettelID'
2015302
2015303
2015304
2015305
$
```

