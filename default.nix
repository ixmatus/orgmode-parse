{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, free, hashable, HUnit, old-locale, stdenv, tasty, tasty-hunit
, text, thyme, unordered-containers
}:
mkDerivation {
  pname = "orgmode-parse";
  version = "0.1.1.3";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers free hashable
    old-locale text thyme unordered-containers
  ];
  testHaskellDepends = [
    aeson attoparsec base bytestring containers hashable HUnit
    old-locale tasty tasty-hunit text thyme unordered-containers
  ];
  description = "A collection of Attoparsec combinators for parsing org-mode flavored documents";
  license = stdenv.lib.licenses.bsd3;
}
