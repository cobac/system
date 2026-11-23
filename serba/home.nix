{ config, pkgs, ... }:

{
  home.username = "coba";
  home.homeDirectory = "/home/coba";

  home.packages = with pkgs; [
    git
    ncdu
    opencode
  ];

  systemd.user.services.opencode = {
    Unit = {
      Description = "OpenCode server";
      After = [ "network-online.target" ];
    };
    Service = {
      Type = "simple";
      ExecStart = "${pkgs.opencode}/bin/opencode serve --service";
      WorkingDirectory = config.home.homeDirectory;
      Restart = "on-failure";
      RestartSec = 2;
    };
    Install.WantedBy = [ "default.target" ];
  };

  home.stateVersion = "25.05";
}
