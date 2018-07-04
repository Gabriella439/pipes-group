let
  default = import ./default.nix;

in
  { inherit (default) pipes-group; }
