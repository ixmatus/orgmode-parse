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
meta-data. Here is a list of all the syntax features that have a complete
parsing implementation and not:

- [X] Headlines
  - [X] State keywords
  - [X] Priority indicator
  - [X] Title
  - [X] Status / progress indicator
  - [X] Tag list
- [X] Property drawers
- [X] State keyword changelogs
- [X] Scheduled and deadline timestamps (timestamp, range, duration, periodicity)
  - [X] Active and inactive timestamps
- [X] Clock timestamps
- [-] Markup
  - [-] Emphasis
    - [X] Bold
    - [X] Italic
    - [X] Strikethrough
    - [X] Underline
    - [ ] Superscript
    - [ ] Subscript
    - [X] Code / monospaced
  - [ ] Tables
  - [-] Lists
    - [X] Unordered lists
    - [X] Numbered lists
    - [ ] Checkbox modified lists
  - [ ] Blocks (src / quote / example blocks)
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
