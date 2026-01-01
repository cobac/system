{
  description = "Personal server config.";
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
  };

  outputs =
    inputs@{ self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
    in
    {
      formatter.x86_64-linux = nixpkgs.legacyPackages.${system}.nixfmt-tree;
      nixosConfigurations = {
        serba = nixpkgs.lib.nixosSystem {
          modules = [
            ./configuration.nix
            inputs.disko.nixosModules.disko
            inputs.home-manager.nixosModules.home-manager
          ];
        };
      };
    };

}
