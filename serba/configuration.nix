{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ./disko-config.nix
    ];
  boot.loader.grub.enable = true;
  networking.hostName = "serba";
  time.timeZone = "Europe/Amsterdam";

  users.users.coba = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE3KLmITLqzVBObYvVrzQGrCo1NMA69ptC8FO+Glvwaf coba"];
    initialHashedPassword = "$y$j9T$WAs6YSAnCkRtfcVRCtSim1$ccd88Zml3nw024IXcR02E1IkKvw.ddDs09ERfSo37W8"; # cottage-core
  };

  users.users.root = {
    openssh.authorizedKeys.keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE3KLmITLqzVBObYvVrzQGrCo1NMA69ptC8FO+Glvwaf coba"];
  };

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "yes";
      AllowUsers = [ "coba" "root" ];
    };
  };

  system.autoUpgrade = {
    enable = true;
    allowReboot = true;
    dates = "daily";
  };
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  programs.neovim = {
    enable = true;
    defaultEditor = true;
  };

  system.stateVersion = "25.05";
}
