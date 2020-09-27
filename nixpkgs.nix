let
  nixpkgsRev = "236eb73d1bd3";
  nixpkgsSrc = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = "04vk87j2nfj9wfhzb830nli3sjif0a7jpklhx7pl7vzgc74zdajv";
  };
in 
  nixpkgsSrc

