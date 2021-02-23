---
slug: syntax-highlighting
---

Syntax highlighting is builtin. To activate highlighting, your [fenced code blocks](https://help.github.com/en/github/writing-on-github/creating-and-highlighting-code-blocks#fenced-code-blocks) must contain a language identifier (see [example](https://help.github.com/en/github/writing-on-github/creating-and-highlighting-code-blocks#syntax-highlighting)).

## Example

Tip: you can click the edit icon in the footer to the view source of this zettel.

Haskell:

```haskell
module Prime where 

primes = filterPrime [2..]
  where 
    filterPrime (p:xs) =
      p : filterPrime [x | x <- xs, x `mod` p /= 0]
```

JSON:

```json
{ "_comment" : "This is JSON"
, "name" : "srid"
, "loc" : "Quebec"
}
```

Nix:

```nix
buildPythonPackage rec {
  pname = "hello";
  version = "1.0";
  src = fetchPypi {
    inherit pname version;
    sha256 = "01ba..0";
  };
}
```

Markdown:

```markdown
This is `markdown`, the *format* used by **Neuron**

# Heading

- Link to [neuron](https://neuron.zettel.page)
- [[Wiki Link]] is not official Markdown syntax
```

Matlab:

```matlab
% This is Matlab code
data = T(:,{'Year','Month','DayofMonth','UniqueCarrier'});
data.Date = datetime(data.Year,data.Month,data.DayofMonth);
data.UniqueCarrier = categorical(data.UniqueCarrier);
```

This one has no language identifier specified:

```
Just plain text.

No particular syntax.
```

## Languages supported

See [here](https://github.com/jgm/skylighting/tree/master/skylighting-core/xml) for a full list of languages supported.
