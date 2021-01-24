# Querying with JSON output

Use the `query` command to query your Zettelkasten and return the matches in JSON format. 

```bash
# Returns all zettels
neuron query
```

Use `jq` to extract needed information from the JSON output. For example, to
extract only the IDs:

```bash
$ neuron query | jq -r '.result | .vertices | .[] | .ID'
index
Tag Queries
extras
$
```
