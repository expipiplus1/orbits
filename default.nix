{ mkDerivation, ad, base, checkers, directory, doctest, exact-real
, filepath, linear, QuickCheck, random, safe, stdenv, tagged, tasty
, tasty-quickcheck, tasty-th, uom-plugin
}:
mkDerivation {
  pname = "orbit";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [ ad base exact-real linear uom-plugin ];
  testHaskellDepends = [
    ad base checkers directory doctest exact-real filepath linear
    QuickCheck random safe tagged tasty tasty-quickcheck tasty-th
    uom-plugin
  ];
  homepage = "https://github.com/expipiplus1/orbit";
  description = "Types and functions for Kepler orbits";
  license = stdenv.lib.licenses.mit;
}
