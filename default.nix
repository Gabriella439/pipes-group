{ mkDerivation, base, doctest, free, lens-family-core, pipes
, pipes-parse, stdenv, transformers
}:
mkDerivation {
  pname = "pipes-group";
  version = "1.0.7";
  src = ./.;
  libraryHaskellDepends = [
    base free pipes pipes-parse transformers
  ];
  testHaskellDepends = [ base doctest lens-family-core ];
  description = "Group streams into substreams";
  license = stdenv.lib.licenses.bsd3;
}
