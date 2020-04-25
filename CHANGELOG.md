# Change Log for neuron

## 0.3.0.0 (DEV)

- New features
  - [Short links](https://neuron.zettel.page/2014501.html).
  - Full text search: `neuron search --full-text`
  - #43: Add `neuron search -e` to open the matching zettel in $EDITOR
  - Allow customizing output directory
  - Added `neuron open` to open the locally generated Zettelkasten site.
  - Custom themes for web interface
  - Custom alias redirects
  - #90: Client-side web search
  - #107: Add full path to the zettel in `neuron query` JSON
  - #115: Hierarchical tags, with tag pattern in zquery
- CLI revamp
  - Zettelkasten directory is now provided via the `-d` argument.
    - Its default, `~/zettelkasten`, is used when not specified.
    - This directory must exist, otherwise neuron will error out.
  - The output directory is now moved to `.neuron/output` under the Zettelkasten directory.
  - `neuron ... rib serve` is now `neuron rib -wS`.
- Bug fixes
  - Fix regression in neuron library use
  - #130: Handle links inside blockquotes
- UI
  - Tags are restyled and positioned below
  - Produce compact CSS in HTML head
  - #24: zquery is displayed in HTML view.
  - #100: Tables are styled nicely using Semantic UI

## 0.2.0.0

- Initial public release
