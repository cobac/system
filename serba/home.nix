{ config, pkgs, ... }:

{
  home.username = "coba";
  home.homeDirectory = "/home/coba";

  home.packages = with pkgs; [
    git
    ncdu
  ];

  home.stateVersion = "25.05";
}
