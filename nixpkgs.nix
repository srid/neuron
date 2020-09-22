let
  nixpkgsRev = "72b9660dc18b";
  nixpkgsSrc = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = "1cqgpw263bz261bgz34j6hiawi4hi6smwp6981yz375fx0g6kmss";
  };
in 
  nixpkgsSrc

