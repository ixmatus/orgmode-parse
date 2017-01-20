# Welcome!
[https://hackage.haskell.org/package/orgmode-parse](https://img.shields.io/hackage/v/orgmode-parse.svg?style=flat)
[https://travis-ci.org/digitalmentat/orgmode-parse](https://travis-ci.org/digitalmentat/orgmode-parse.svg?branch=master)

`orgmode-parse` provides a top-level parser and collection of attoparsec parser
combinators for org-mode structured text.

- [#whats-finished](What's Finished)
- [#whats-planned](What's Planned)
- [#building](Building)
  
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
- [ ] Markup
  - [ ] Emphasis
    - [ ] Bold
    - [ ] Italic
    - [ ] Strikethrough
    - [ ] Underline
    - [ ] Superscript
    - [ ] Subscript
    - [ ] Code / monospaced
  - [ ] Tables
  - [ ] Lists
    - [ ] Unordered lists
    - [ ] Numbered lists
    - [ ] Checkbox modified lists
  - [ ] Blocks (src / quote / example blocks)

Parsing org-mode markup is currently being worked on.

## What's Planned (outside of what's not finished)
1. Modernizing this library and adding significantly more documentation to it
2. Writing a sister library, `orgmode-pretty`, providing a pretty printer
   implementation for an org-mode AST
3. Pandoc integration

## Building
There are a few ways to build this library if you're developing a patch:

- `stack build && stack test`, and
- `nix-build -A orgmode-parse release.nix`

Generally, you should be using the ~stack build~ workflow but if you're on a
NixOS system or have Nix installed and wish to test a Nix build (at the time of
this writing, NixOS 16.09 has been used to build with) you can do so with the
above ~nix-build~ invocation.

# License
[BSD3 Open Source Software License](https://github.com/digitalmentat/orgmode-parse/blob/master/LICENSE)
