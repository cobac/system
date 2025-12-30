{ config, pkgs, ... }:

{
  home.username = "coba";
  home.homeDirectory = "/home/coba";

  home.packages = with pkgs; [
    git
  ];

  services.syncthing.enable = true;

  home.stateVersion = "25.05";
}
