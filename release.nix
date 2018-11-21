{ compiler ? "ghc844" }:

let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
    rev          = "bedbba61380a4da0318de41fcb790c176e1f26d1";
    sha256       = "0z4fgh15nz86kxib9ildmh49v6jim6vgbjyla7jbmgdcl0vd9qsg";
    outputSha256 = "0dxxw2ipa9403nk8lggjsypbr1a9jpb3q4hkjsg89gr5wz26p217";
  };

  overlays = [
    (newPkgs: oldPkgs: {
      haskell = oldPkgs.haskell // {
        packages = oldPkgs.haskell.packages // {
          "${compiler}" = oldPkgs.haskell.packages."${compiler}".override {
            overrides = (haskellPackagesNew: haskellPackagesOld: {
              orgmode-parse = haskellPackagesNew.callCabal2nix "orgmode-parse" ./. { };
            });
          };
        };
      };
    })
  ];

  pkgs = import nixpkgs { inherit overlays; };

in

  { orgmode-parse = pkgs.haskell.packages."${compiler}".orgmode-parse;}
