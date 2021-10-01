{ config, lib, pkgs, ... }:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
  ;

  cfg = config.services.neuron;
in
{
  options = {
    services.neuron = {
      enable = mkEnableOption ''
        Future-proof note-taking and publishing based on Zettelkasten
      '';

      package = mkOption {
        type = types.package;
        default = pkgs.neuron-notes;
        defaultText = "pkgs.neuron-notes";
        description = "neuron derivation to use";
      };

      notesDirectory = mkOption {
        type = types.nullOr types.path;
        default = null;
        example = "/home/\${name}/zettelkasten";
        description = "Directory that holds the neuron notes";
      };

      host = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = "The hostname or IP address the HTTP server should listen to";
      };

      port = mkOption {
        type = types.port;
        default = 8080;
        description = "Port the HTTP server should listen to";
      };

      prettyUrls = mkOption {
        type = types.bool;
        default = false;
        description = "If set, drops the .html at the end of Zettel URLs.";
      };

      systemdTarget = mkOption {
        type = types.str;
        default = "graphical-session.target";
        description = "Systemd target to bind to";
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.notesDirectory != null;
        message = "`services.neuron.notesDirectory' must be set.";
      }
    ];

    systemd.user.services.neuron = {
      Unit = {
        Description = "Neuron zettelkasten service";
        PartOf = [ cfg.systemdTarget ];
        After = [ cfg.systemdTarget ];
      };

      Service = {
        Type = "exec";
        ExecStart = ''
          ${cfg.package}/bin/neuron \
            -d ${cfg.notesDirectory} \
            gen \
            --watch \
            --serve ${cfg.host}:${toString cfg.port} \
            ${lib.optionalString cfg.prettyUrls "--pretty-urls"}
        '';
      };

      Install = {
        WantedBy = [ cfg.systemdTarget ];
      };
    };
  };
}
