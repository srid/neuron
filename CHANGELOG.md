# Change Log for neuron

## 0.3.0.0 (DEV)

- Bug fixes
  - Fix regression in neuron library use
- UI
  - Tags are restyled and positioned below
  - Produce compact CSS in HTML head
  - #24: zquery is displayed in HTML view.
- CLI revamp
  - Zettelkasten directory is now provided via the `-d` argument.
    - Its default, `~/zettelkasten`, is used when not specified.
    - This directory must exist, otherwise neuron will error out.
  - The output directory is now moved to `.neuron/output` under the Zettelkasten directory.
  - Added `neuron open` to open the locally generated Zettelkasten site.
  - `neuron ... rib serve` is now `neuron rib -wS`.
- Support for [short links](https://neuron.srid.ca/2014501.html).

## 0.2.0.0

- Initial public release
