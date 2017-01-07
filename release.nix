let config = {
  allowUnfree = true;
  packageOverrides = pkgs: {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: {
        orgmode-parse = haskellPackagesNew.callPackage ./default.nix { };
      };
    };
  };
};
in

{ pkgs ? import <nixpkgs> { inherit config; } }:
let
  darwinPkgs = import <nixpkgs> { inherit config; system = "x86_64-darwin"; };
  linuxPkgs  = import <nixpkgs> { inherit config; system = "x86_64-linux" ; };
  pkgs       = import <nixpkgs> { inherit config; };

in
  { orgmode-parse-linux  =  linuxPkgs.haskellPackages.orgmode-parse;
    orgmode-parse-darwin = darwinPkgs.haskellPackages.orgmode-parse;
    orgmode-parse        =       pkgs.haskellPackages.orgmode-parse;
  }
