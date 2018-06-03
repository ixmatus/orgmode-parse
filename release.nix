let

  config   = { allowUnfree = true; };
  overlays = [
    (newPkgs: oldPkgs: rec {

      haskellPackages = oldPkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          orgmode-parse = haskellPackagesNew.callPackage ./default.nix { };
        };
      };

    })
  ];

  nixpkgs = import ./nix/17_09.nix;
  pkgs    = import nixpkgs { inherit config overlays; };

  darwinPkgs = import nixpkgs { inherit config overlays; system = "x86_64-darwin"; };
  linuxPkgs  = import nixpkgs { inherit config overlays; system = "x86_64-linux" ; };

in
  { orgmode-parse-linux  =  linuxPkgs.haskellPackages.orgmode-parse;
    orgmode-parse-darwin = darwinPkgs.haskellPackages.orgmode-parse;
    orgmode-parse        =       pkgs.haskellPackages.orgmode-parse;
  }
