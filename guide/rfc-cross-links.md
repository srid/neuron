# Cross-linking

Cross linking lets you compose zettelkastens via "namespaced links". Consider these scenarios:

- I want to publish a subset of my zettelkastens publically (as a blog) without having to worry about leaking private data. I can move my "public" zettels to a new zettelkasten at `public/` and have `neuron.dhall` point to it as `blog`. My private zettelkasten thus knows all about the public zettelkasten while my public zettelkasten has no idea about any other zettels besides the one in `public/`.
- We have two separate zettelkastens situated at `home/` and `work/`. We want to "compose" these to form a larger zettelkasten composing of zettels from both of these without worrying about zettel name conflicts and also maintaining the directory structure. We can have `./neuron.dhall` point to `home/` as `home` and `work/` as `work` and in doing so we have (very effortlessly) merged these zettelkastens without any conflicts.

## Design

Use the `cross-links` configuration value to create a mapping between alias names and the zettelkasten directory you want to cross-link (Note that this path must be relative). For example -

```dhall
{ siteTitle = "My local Zettelkasten"
, siteBaseUrl = Some "127.0.0.1:8080"
, cross-links = { home = "home/", work = "work/"}
}
```

- All zettels in the directory mapped to `home` must be prefixed with `home:` when accessing from this zettelkasten (similarly for `work/`).
- In the output, all zettels from the directory mapped to `home` will be generated under `output/home`.
- A zettelkasten can only access cross-links defined in its own `neuron.dhall`.
- If a name is mapped to a directory in a cross link, it should not be mapped to a different directory in any zettelkasten that is directly or indirectly cross-linked with the current zettelkasten.
- Thus cross link names are not globally accessible but they are globally uniquely mapped.
- You don't need to do anything special to query cross-linked zettels.
