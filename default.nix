let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;

  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/391f93a83c3a486475d60eb4a569bb6afbf306ad.tar.gz";
    sha256 = "0s5f7j2akh3g0013880jfbigdaac1z76r9dv46yw6k254ba2r6nq";
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
              (pkgs.haskell.lib.packageSourceOverrides { pipes-group = ./.; })
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
