let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;

  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/ae66c3e40486c0e88a6cefc8d275c248fc6a696c.tar.gz";
    sha256 = "1gw4kdlkmxyil8capnagv41hqmh31hkibidjgy3bxhlljr8xgfkc";
  };

  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides =
          let
            manualOverrides = haskellPackagesNew: haskellPackagesOld: {
            };

          in
            pkgs.lib.composeExtensions
              (pkgs.haskell.lib.packagesFromDirectory { directory = ./nix; })
              manualOverrides;
      };
    };
  };

  pkgs =
    import nixpkgs { inherit config; };

in
  { inherit (pkgs.haskellPackages) pipes-group;

    shell = (pkgs.haskell.lib.doBenchmark pkgs.haskellPackages.pipes-group).env;
  }
