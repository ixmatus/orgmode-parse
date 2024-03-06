{
  description = "orgmode-parse for all your org-mode needs";

  inputs = {
    # Nix Inputs
    nixpkgs.url = "github:nixos/nixpkgs/?ref=nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
    hs-flake-utils.url = "git+https://whetstone.private.storage/shae/hs-flake-utils.git?ref=main";
    hs-flake-utils.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    hs-flake-utils,
  }: let
    ulib = flake-utils.lib;
    ghcVersion = "ghc96"; # nixos 23.11 gives ghc 9.6.3
  in
    ulib.eachSystem ["x86_64-linux" "aarch64-darwin"] (system: let
      # Get a nixpkgs customized for this system
      pkgs = import nixpkgs {
        inherit system;
      };
      hslib = hs-flake-utils.lib {
        inherit pkgs;
        src = ./.;
        compilerVersion = ghcVersion;
        packageName = "orgstat";
      };
    in {
      checks = hslib.checks {};
      devShells = hslib.devShells {
        extraBuildInputs = pkgs:
          with pkgs; [
            haskell-language-server
          ];
      };
      packages = hslib.packages {};
    });
}
