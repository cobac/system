{ config, pkgs, ... }:

{
  home.username = "coba";
  home.homeDirectory = "/home/coba";

  home.packages = with pkgs; [
    git
  ];

  home.stateVersion = "25.05";
}
