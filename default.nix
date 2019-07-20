let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;

  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/3e5cf4fa63552795ed7fa6ee160de4b3b1ef5753.tar.gz";
    sha256 = "0a60l2jrplgfrfhw40cyr73nksb2bdjl0kxgs6i7nqilxfdd3kb6";
  };

  readDirectory = import ./nix/readDirectory.nix;

  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides =
          let
            manualOverrides = haskellPackagesNew: haskellPackagesOld: {
            };

          in
            pkgs.lib.composeExtensions (readDirectory ./nix) manualOverrides;
      };
    };
  };

  pkgs =
    import nixpkgs { inherit config; };

in
  { inherit (pkgs.haskellPackages) pipes-group;

    shell = (pkgs.haskell.lib.doBenchmark pkgs.haskellPackages.pipes-group).env;
  }
