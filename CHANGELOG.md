# Change Log for neuron

## Unreleased

- Fix search.html to handle zettels with title IDs (with whitespace) (#438)
- `neuron search`
  - Revert #429 for neuron-search regression
  - Deal with title IDs in search (#445, #439)
- Web interface
  - Simplify error message UX for missing wiki-links (#448)
  - Baclinks surrounding context (#190)
  - Fix blockquote paragraph order in footnotes (#350)
  - Allow specifying URL slugs in notes (#483)
  - Fix pipe character (`|`) messing up Markdown parsing (#469, #465)
- Zettel format
  - Ignore punctuation in inline tags (#443)
  - Remove support for autolinks (`<..>`) (#449) (see [migration script](https://github.com/srid/neuron/issues/449#issuecomment-719062302))
    - Raw HTML now works without any special syntax.
  - Add Markdown highlighting extension (#453)
- Unicode-aware in inline tags (#446)
- CLI:
  - `neuron open --id` is replaced by `neuron open --slug` (#483)
- Removed features
  - Aliases (#479)

## 1.0.1.0

- Fix broken `neuron search` in static binary (#429)

## 1.0.0.0

- Updates
  - Bump commonmark, commonmark-extensions (0.2)
  - Skylighting 0.9 (pure Haskell syntax highlighting)
- Web Interface
  - Styling fixes
    - Fix incorrect body font (esp. on Windows)
    - Fix task list checkbox styling (#233)
    - Use superscript for folgezettel link suffix (#346)
  - Fix missing clusters in z-index on some cyclic graphs (#357)
  - Update Fomantic UI to 2.8.7
  - Allow custom HTML snippet in `<head>` (#385)
  - Group orphan zettels in z-index (#380)
  - Fix footnote alignment on Firefox (#398)
  - Hide footnote refs from search result preview (#326)
  - Backlinks improvements
    - Backlinks panel now displays all kinds of backlinks (includig uplinks) for sake of completion.
- Zettel format
  - Wiki-style links: instead of `<foo>` you can now use `[[[foo]]]`; and instead of `<foo?cf>`, you can use `[[foo]]`. (#351)
  - Support regular markdown links (#366)
  - Allow title as Zettel IDs (#399)
  - Drop support for legacy links (#366)
  - Allow specifying time in the `date` metadata propery (#343)
  - Add `unlisted` metadata property to hide a zettel from z-index (#318)
  - Markdown:
    - Inline tags (#189)
    - support for [fancy lists](https://github.com/jgm/commonmark-hs/blob/master/commonmark-extensions/test/fancy_lists.md) (#335)
    - Fix hard line breaks to actually work (#354)
  - Allow dot in Zettel ID (#369)
  - Drop support for legacy date IDs (#368)
  - Add [`bracked_spans`](https://github.com/jgm/commonmark-hs/blob/master/commonmark-extensions/test/bracketed_spans.md) extension (#406)
- CLI
  - Faster querying: add `--cached` option to `neuron query`, to run faster using the cache. To keep the cache up to date, make sure that `neuron rib` is running.
  - Add `--id` and `--search` options to `open` command to open given zettel ID or search page respectively  (#317)
  - Use current directory as the default value for `-d` (a la. git) (#389)
  - Adapt `neuron new` for title IDs (#408)
  - `neuron search`: fail if org is enabled, unless --full-text is used. (#420)
- Static binaries
- Unicode-aware in filenames and links (#424)

## 0.6.0.0

- Markdown: switch to Pandoc, and commonmark (with [extensions](https://github.com/jgm/commonmark-hs/tree/master/commonmark-extensions)). #166
  - Markdown parsing is consequently less strict and more permissive
  - With this change, neuron can potentially support other text formats (experimental org support already in)
  - Raw HTML support (#191)
  - YAML block is now optional; title is also optional, while native Markdown H1 titles are now supported (#230)
- Web Interface
  - Introduce new "uplink tree" view, replacing connections pane (#195)
  - Allow customizing favicon
  - z-index: Allow cycles (#248)
  - z-index: display parse and query errors (#220, #221)
- CLI:
  - Resilient error handling (#202, #215)
  - Added `neuron query --graph` to get the entire graph as JSON export
    - Add backlinks query (#216)
  - Add bash/zsh shell completion (#239)
  - neuron new: title is optional (#232)
- Bug fixes
  - Fix 'neuron new' generating invalid Markdown when title contains special characters (#163)
  - Allow custom CLI in $EDITOR (#227)
- Others
  - Reduce install size (#240)
  - Nightly docker releases 
  - Automatic publishing through [neuron-template](https://github.com/srid/neuron-template)

## 0.4.0.0

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
  - Display all backlinks to a zettel (even those not in folgezettel heterarchy) (#34)
  - Simplified link style (#151)
  - Client-side web search (#90)
  - Add JSON-LD breadcrumbs (#147)
  - Add query to display tag tree (#121)
  - Custom alias redirects
  - Tags are restyled and positioned below
  - Produce compact CSS in HTML head
  - #24: zquery is displayed in HTML view.
  - #100: Tables are styled nicely using Semantic UI
- Bug fixes & misc changes
  - Fix regression in neuron library use
  - #130: Handle links inside blockquotes
  - Allow neuron to utilize multiple CPUs (#171)

## 0.2.0.0

- Initial public release
