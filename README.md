# Welcome!
![Hackage Version](https://img.shields.io/hackage/v/orgmode-parse.svg?style=flat)
![Travis CI Status](https://travis-ci.org/ixmatus/orgmode-parse.svg?branch=master)

`orgmode-parse` provides a top-level parser and collection of attoparsec parser
combinators for org-mode structured text.

- [What's Finished](#whats-finished)
- [Building](#building)

You can find the package on [Hackage](https://hackage.haskell.org/package/orgmode-parse).
  
## What's Finished
We have built attoparsec parsers for parsing org-mode document structures and
meta-data. Here is a list of [all the syntax features](https://orgmode.org/worg/dev/org-syntax.html) that have a complete
parsing implementation and not:

- [X] Headlines and Sections
- [ ] Affiliated Keywords
- [-] Greater Elements
  - [ ] Greater Blocks
  - [X] Drawers 
  - [ ] Dynamic Blocks
  - [ ] Footnote Definitions
  - [ ] Inlinetasks
  - [ ] Plain Lists and Items
    - [X] Unordered lists
    - [X] Numbered lists
    - [ ] Checkbox modified lists
  - [X] Property Drawers
  - [ ] Tables
- [ ] Elements
  - [ ] Babel Cell
  - [ ] Blocks
  - [X] Clock, Diary Sexp and Planning
    - [X] Scheduled and deadline timestamps (timestamp, range, duration, periodicity)
      - [X] Active and inactive timestamps
    - [X] Clock timestamps
  - [ ] Comments
  - [ ] Fixed Width Areas
  - [ ] Horizontal Rules
  - [X] Keywords
  - [ ] LaTeX Environments
  - [X] Node Properties
  - [X] Paragraphs
  - [ ] Table Rows
- [ ] Objects
  - [-] Entities and LaTeX Fragments
  - [ ] Export Snippets
  - [ ] Footnote References
  - [ ] Inline Babel Calls and Source Blocks
  - [ ] Line Breaks (\\)
  - [ ] Links
  - [ ] Macros
  - [ ] Targets and Radio Targets
  - [ ] Statistics Cookies
  - [ ] Table Cells
  - [-] Timestamps
  - [ ] Text Markup
    - [X] Bold
    - [X] Italic
    - [X] Strikethrough
    - [X] Underline
    - [ ] Superscript
    - [ ] Subscript
    - [X] Code / monospaced
- [ ] Position Annotated AST

## Building
There are a few ways to build this library if you're developing a patch:

- `stack build && stack test`, and
- `nix-build --no-out-link --attr orgmode-parse release.nix`

You can also use the `nix-shell` provided cabal environment for incremental
development:

```shell
$ nix-shell
$ cabal build
```

## Projects that use this package:
https://github.com/volhovm/orgstat

# License
[BSD3 Open Source Software License](https://github.com/digitalmentat/orgmode-parse/blob/master/LICENSE)
