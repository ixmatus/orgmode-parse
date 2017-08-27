0.2.0
- Fix timestamp parsing in headline and body, fixes #13
- Generalize drawer parser for logbook and generic drawers, fixes #14
- Reorganize the types, fixes #15

0.1.1.0
- The weekday parser now correctly parses weekday appellations of other
  languages (thank you nushio3!) using a combinator-style version of the regex
  found in org-mode.

0.1.0.4
- Comment improvement wibbles.
- Adding the =Attoparsec= combinator modules to the export module list in the
  cabal package definition.

0.1.0.3
- Sub-headings are now parsed and tracked by its parent.
- Much more robust timestamp / clock / schedule parsing.
- A good mount of code cleanup and comment improvement.

0.0.2.1
- [X] Fixing the import for the =Internal= module (instead of re-exporting it in
  each parser module).

0.0.2.0
- [X] Added parsers for the scheduled / deadline timestamps.

0.0.1.1
- [X] Parsing of property drawer.
- [X] Tests for both drawer and heading parsers.

0.0.0.2
- [X] Parsers for orgmode list headlines.
