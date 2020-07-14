# `home-manager` systemd service

If you use [home-manager](https://github.com/rycee/home-manager), you can have
`neuron` run in the background automatically. Add the following to your `home.nix`:

```nix
systemd.user.services.neuron = let
  notesDir = "/path/to/your/zettelkasten";
  # See "Declarative Install"
  neuron = (
    let neuronRev = "GITREVHERE";
        neuronSrc = builtins.fetchTarball https://github.com/srid/neuron/archive/${neuronRev}.tar.gz;
     in import neuronSrc {});
in {
  Unit.Description = "Neuron zettelkasten service";
  Install.WantedBy = [ "graphical-session.target" ];
  Service = {
    ExecStart = "${neuron}/bin/neuron -d ${notesDir} rib -wS";
  };
};
```
