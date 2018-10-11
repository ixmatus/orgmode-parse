{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, free, hashable, HUnit, neat-interpolation, old-locale, semigroups
, stdenv, tasty, tasty-hunit, text, thyme, unordered-containers
}:
mkDerivation {
  pname = "orgmode-parse";
  version = "0.2.2";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers free hashable
    old-locale semigroups text thyme unordered-containers
  ];
  testHaskellDepends = [
    aeson attoparsec base bytestring containers free hashable HUnit
    neat-interpolation old-locale semigroups tasty tasty-hunit text
    thyme unordered-containers
  ];
  description = "A collection of Attoparsec combinators for parsing org-mode flavored documents";
  license = stdenv.lib.licenses.bsd3;
}
