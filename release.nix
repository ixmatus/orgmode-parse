let

  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;

  nixpkgs = fetchNixpkgs {
    rev          = "def2618c098fad7db233117dc06ce3eb7be93e2d";
    sha256       = "06hf9rcilvpl7shmmkppaa7c45jnfcsk8850c8zxr369vzlwdrpq";
    outputSha256 = "1654yc4xyacb28ixsia1i085g7sbvgmil2x7bn1faqp670n42hjr";
  };

  config   = { allowUnfree = true; };
  overlays = [
    (newPkgs: oldPkgs: rec {

      haskellPackages = oldPkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          orgmode-parse = haskellPackagesNew.callCabal2nix "orgmode-parse" ./. { };
        };
      };

    })
  ];

  pkgs = import nixpkgs { inherit config overlays; };

  darwinPkgs = import nixpkgs { inherit config overlays; system = "x86_64-darwin"; };
  linuxPkgs  = import nixpkgs { inherit config overlays; system = "x86_64-linux" ; };

in
  { orgmode-parse-linux  =  linuxPkgs.haskellPackages.orgmode-parse;
    orgmode-parse-darwin = darwinPkgs.haskellPackages.orgmode-parse;
    orgmode-parse        =       pkgs.haskellPackages.orgmode-parse;
  }
