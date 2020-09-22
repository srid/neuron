let
  nixpkgsRev = "6d4b93323e7f";
  nixpkgsSrc = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = "0g2j41cx2w2an5d9kkqvgmada7ssdxqz1zvjd7hi5vif8ag0v5la";
  };
in 
  nixpkgsSrc

