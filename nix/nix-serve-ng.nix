{ mkDerivation, base, bsd-sysctl, bytestring, http-types, lib
, managed, megaparsec, network, nixstore, nixutil
, optparse-applicative, vector, wai, warp, warp-tls
}:
mkDerivation {
  pname = "nix-serve-ng";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bsd-sysctl bytestring http-types managed megaparsec network
    optparse-applicative vector wai warp warp-tls
  ];
  executableSystemDepends = [ nixstore nixutil ];
  description = "A drop-in replacement for nix-serve that's faster and more stable";
  license = lib.licenses.bsd3;
}
