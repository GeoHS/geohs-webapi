{ mkDerivation, aeson, base, base64-bytestring, bytestring
, geohs-geometry, hspec, http-api-data, lens, neat-interpolation
, optparse-applicative, purescript-bridge, QuickCheck
, quickcheck-instances, servant, servant-auth, servant-auth-server
, servant-foreign, servant-purescript, servant-swagger
, spatial-reference, stdenv, swagger2, text, wai-ogc, attoparsec
}:
mkDerivation {
  pname = "geohs-webapi";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  hyperlinkSource = false;
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring geohs-geometry
    http-api-data lens neat-interpolation purescript-bridge servant
    servant-auth servant-auth-server servant-foreign servant-purescript
    servant-swagger spatial-reference swagger2 text wai-ogc attoparsec
  ];
  executableHaskellDepends = [
    base lens optparse-applicative purescript-bridge servant-purescript
    spatial-reference
  ];
  testHaskellDepends = [
    aeson base hspec lens QuickCheck quickcheck-instances
    spatial-reference swagger2
  ];
  homepage = "https://github.com/GeoHS/geohs-webapi";
  description = "GeoHS web api";
  license = stdenv.lib.licenses.bsd3;
}
