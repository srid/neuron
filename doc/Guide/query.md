# Querying with JSON output

Use the `query` command to query your Zettelkasten and return the matches in JSON format. 

## Querying all zettels

To retrieve the metadata (sans content) of all zettels in your Zettelkasten:

```bash
neuron query --zettels
```

You can use [`jq`][jq] to further process the JSON result. For eg., to print a list of zettel titles:

```bash
 neuron query --zettels | jq ".[].Title"
 ```

## Querying a single zettel

To retrieve the metadata for a zettel by its [[id]]:

```bash
neuron query --id=index
```

## Querying entire Zettelkasten graph

```bash
neuron query --graph
```

## Other queries

- `neuron query --backlinks-of ID`
- `neuron query --uplinks-of ID`
- `neuron query --tags` ([[Tags]] must be enabled)

## Fast querying

Pass the `--cached` argument if you want the query to run instantly, by reading from the local cache. To make sure that the cache remains up-to-date, you must be running `neuron gen` as a daemon.

[jq]: https://stedolan.github.io/jq/