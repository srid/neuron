# Change Log for neuron

## UNRELEASED

- Notable changes
  - More convenient links; `<foobar>` instead of `[foobar](z:/)` (#59)
  - Hierarchical tags, with tag pattern in zquery (#115)
  - Change default ID generation to use random hash (#151)
    - Add `date` field to Markdown metadata
  - Added `neuron query` to query the Zettelkasten and get JSON output (#42)
- CLI argument parsing revamp
  - Zettelkasten directory is now provided via the `-d` argument.
    - Its default, `~/zettelkasten`, is used when not specified.
    - This directory must exist, otherwise neuron will error out.
  - The output directory is now moved to `.neuron/output`
  - `neuron ... rib serve` is now `neuron rib -wS`.
- CLI changes:
  - Full text search: `neuron search --full-text`
  - #43: Add `neuron search -e` to open the matching zettel in $EDITOR
  - Allow customizing output directory
  - Added `neuron open` to open the locally generated Zettelkasten site.
  - #107: Add full path to the zettel in `neuron query` JSON
- Web interface:
  - Custom themes for web interface
  - Display all backlinks to a zettel (even those not in category tree) (#34)
  - Simplified link style (#151)
  - Client-side web search (#90)
  - Add JSON-LD breadcrumbs (#147)
  - Add query to display tag tree (#121)
  - Custom alias redirects
  - Tags are restyled and positioned below
  - Produce compact CSS in HTML head
  - #24: zquery is displayed in HTML view.
  - #100: Tables are styled nicely using Semantic UI
- Bug fixes
  - Fix regression in neuron library use
  - #130: Handle links inside blockquotes

## 0.2.0.0

- Initial public release
